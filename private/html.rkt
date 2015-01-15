#lang racket/base
(provide generate-html-coverage)
(require racket/file
         racket/path
         racket/format
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         racket/string
         syntax/modread
         syntax/parse
         unstable/sequence
         (only-in xml write-xexpr)
         "format-utils.rkt"
         "shared.rkt")


(module+ test
  (require rackunit "../cover.rkt" racket/runtime-path racket/set)
  (define-runtime-path root "..")
  (define-runtime-path tests/basic/prog.rkt "../tests/basic/prog.rkt"))

;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage [d "coverage"])
  (define dir (simplify-path d))
  (define fs (get-files coverage dir))
  (write-files fs)
  (move-support-files! dir))

(define (get-files coverage dir)
  (define file-list
    (for/list ([(k v) coverage])
      (vprintf "building html coverage for: ~a\n" k)
      (define exploded (explode-path k))
      (define-values (_ dir-list)
        (split-at exploded
                  (length (explode-path (current-directory)))))
      (define coverage-dir-list
        (cons dir (take dir-list (sub1 (length dir-list)))))
      (define relative-output-file (path-replace-suffix (last exploded) ".html"))
      (define output-file
        (apply build-path (append coverage-dir-list (list relative-output-file))))
      (define output-dir (apply build-path coverage-dir-list))
      (define path-to-css
        (path->string
         (apply build-path
                (append (build-list (sub1 (length coverage-dir-list))
                                    (const ".."))
                        (list "main.css")))))
      (define xexpr (make-html-file (hash-ref coverage k) k path-to-css))
      (list output-file output-dir xexpr)))
  (define index (generate-index coverage))
  (cons (list (build-path dir "index.html") dir index)
        file-list))

(module+ test
  (test-begin
   (parameterize ([current-directory root])
     (after
      (define f (path->string (simplify-path tests/basic/prog.rkt)))
      (define d "coverage")
      (test-files! f)
      (define coverage (get-test-coverage))
      (define files (get-files coverage d))
      (define (maybe-path->string p)
        (if (string? p) p (path->string p)))
      (check-equal? (list->set (map (compose maybe-path->string first)
                                    files))
                    (set "coverage/index.html"
                         "coverage/tests/basic/prog.html"))
      (check-equal? (list->set (map (compose maybe-path->string second) files))
                    (set "coverage"
                         "coverage/tests/basic"))
      (clear-coverage!)))))

(define (write-files f)
  (for ([l f])
    (match-define (list f d e) l)
    (vprintf "writing html coverage: ~s\n" f)
    (make-directory* d)
    (with-output-to-file f
      #:exists 'replace
      (thunk (write-xexpr e)))))

(define-runtime-path css "main.css")
(define (move-support-files! dir)
  (copy-file css (build-path dir "main.css") #t))

;; FileCoverage PathString PathString -> Xexpr
(define (make-html-file coverage path path-to-css)
  (define covered? (make-covered? coverage path))
  (define cover-info (expression-coverage/file path covered?))
  (define-values (covered total) (values (first cover-info) (second cover-info)))
  `(html ()
    (head ()
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [type "text/css"] [href ,path-to-css])))
    (body ()
          ,(if (zero? total) "No Coverage Information" (%s->xexpr (/ covered total)))
          (div ([class "code"])
               ,(file->html path covered?)))))

(define (%s->xexpr %)
  `(p () ,(~a "expr" ': " " (~r (* 100 %) #:precision 2) "%") (br ())))

(module+ test
  (test-begin
   (define f (path->string (simplify-path tests/basic/prog.rkt)))
   (test-files! f)
   (define cov (hash-ref (get-test-coverage) f))
   (define covered? (make-covered? cov f))
   (check-equal? (make-html-file cov f "main.css")
                 `(html ()
                   (head ()
                         (meta ([charset "utf-8"]))
                         (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
                   (body ()
                         (p () "expr: 100%" (br ()))
                         (div ([class "code"])
                              ,(file->html f covered?)))))
   (clear-coverage!)))

(define (file->html path covered?)
  (define file (file->string path))
  (define-values (lines _)
    (for/fold ([ls null] [pos 1])
              ([line (string-split file "\n")])
      (define-values (rline npos)
        (for/fold ([r null] [pos pos])
                  ([c line])
          (values
           (cons (mode-xml (covered? pos)
                           (encode-char c))
                 r)
           (add1 pos))))
      (values
       (cons `(li () ,@(reverse rline)) ls)
       (add1 npos))))
  `(ol ()
    ,@(reverse lines)))

(define (encode-char c)
  (case c
    [(#\space) 'nbsp]
    [else (string c)]))
(module+ test
  (check-equal? (encode-char #\space)
                'nbsp))

(define (mode-xml mode body)
  (define class
    (case mode
      [(covered) "covered"]
      [(uncovered) "uncovered"]
      [(irrelevant) "irrelevant"]))
  `(span ((class ,class)) ,body))

(module+ test
  (define (test file out)
    (test-files! file)
    (define cov (hash-ref (get-test-coverage) file))
    (define covered? (make-covered? cov file))
    (check-equal? (file->html file covered?)
                  out)
    (clear-coverage!))
  (define f (path->string (simplify-path tests/basic/prog.rkt)))
  (test f
        `(ol ()
          (li ()
              ,@(for/list ([c (first (string-split (file->string f) "\n"))])
                 `(span ((class "covered"))
                   ,(encode-char c))))
          ,@(for/list ([l (rest (string-split (file->string f) "\n"))])
              `(li ()
                ,@(for/list ([c l])
                    `(span ((class ,(if (equal? c #\space) "irrelevant" "covered")))
                      ,(encode-char c))))))))

;; Index File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coverage PathString -> Xexpr
;; Generate the index html page for the given coverage information
(define (generate-index coverage)
  (define expression-coverage (expression-coverage/all coverage))
  `(html
    (head ()
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
    (body ()
          (div ([class "report-container"])
               ,(div:total-coverage expression-coverage)
               ,(div:file-reports expression-coverage)))))

;; [Hash FilePath ExpressionInfo] -> Xexpr
(define (div:total-coverage expr-coverages)
  (define total-coverage-percentage (expression-coverage-percentage/all expr-coverages))
  (define coverage-as-string
    (if (equal? +nan.0 total-coverage-percentage)
        "No Coverage Information"
        (real->decimal-string total-coverage-percentage)))
  `(div ([class "total-coverage"])
        ,(string-append "Total Project Coverage: " coverage-as-string "%")))

;; [Hash FilePath ExpressionInfo] -> Xexpr
(define (div:file-reports expr-coverages)
  `(div ([class "file-list"])
        ,@(for/list ([(path expr-info) expr-coverages])
            (div:file-report path expr-info))))
;; TODO: FIGURE OUT ORDERING ISSUE
#;(module+ test
  (test-begin 
   (check-equal? (div:file-reports (hash "foo.rkt" (list 0 10)
                                         "bar.rkt" (list 10 10)))
                 `(div ()
                       ,(div:file-report "foo.rkt" (list 0 10))
                       ,(div:file-report "bar.rkt" (list 10 10))))))

;; PathString ExpressionInfo -> Xexpr
;; create a div that holds a link to the file report and expression
;; coverage information
(define (div:file-report path expr-coverage-info)
  (define local-file 
    (path->string (find-relative-path (current-directory) (string->path path))))
  (define percentage 
    (cond
      [(zero? (second expr-coverage-info)) "No Coverage Info"]
      [else (real->decimal-string 
             (exact->inexact (* 100 (/ (first expr-coverage-info) 
                                       (second expr-coverage-info)))))]))
  `(div ([class "file-info"])
        (div () (a ([href ,(coverage-report-link path)]) ,local-file))
        (div () ,percentage)
        (div () ,(real->decimal-string (first expr-coverage-info)))
        (div () ,(real->decimal-string (second expr-coverage-info)))))

(module+ test
  (test-begin (check-equal? (div:file-report "foo.rkt" (list 0 0))
                            '(div ((class "file-info"))
                                  (div () (a ((href "foo.html")) "foo.rkt"))
                                  (div () "No Coverage Info")
                                  (div () "0.00")
                                  (div () "0.00"))))
  (test-begin (check-equal? (div:file-report "foo.rkt" (list 10 10))
                            '(div ((class "file-info"))
                                  (div () (a ((href "foo.html")) "foo.rkt"))
                                  (div () "100.00")
                                  (div () "10.00")
                                  (div () "10.00")))))

(define (coverage-report-link path)
  (define local-file (find-relative-path (current-directory) path))
  (path->string (path-replace-suffix local-file ".html")))

;; File Coverage Reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Percentage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Percentage is a Real∈[0,100]

;; [Hash FilePath ExpressionInfo] -> Percentage
;; Get the total expression conversion percentage for the whole project
(define (expression-coverage-percentage/all all-expr-info)
  (define total-covered (for/sum ([v (hash-values all-expr-info)]) (first v)))
  (define total-exprs (for/sum ([v (hash-values all-expr-info)]) (second v)))
  (if (zero? total-exprs)
      +nan.0
      (* (/ total-covered total-exprs) 100)))

(module+ test
  (test-begin 
   (check-equal? 
    (expression-coverage-percentage/all (hash "foo.rkt" (list 0 10)
                                              "bar.rkt" (list 10 10)))
    50)))

;; Expression Coverage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ExpressionInfo is a (List Nat Nat) where:
;; the first element is the number of covered expressions
;; the second element is the total number of expressions

;; Coverage -> [Hash FilePath ExpressionInfo]
;; returns a hash that maps file paths to an ExpressionInfo
(define (expression-coverage/all coverage)
  (for/hash ([(file data) coverage])
    (values file (expression-coverage/file file (make-covered? data file)))))

;; FilePath Covered? -> ExpressionInfo
;; Takes a file path and a Covered? and 
;; gets the number of expressions covered and the total number of expressions.
(define (expression-coverage/file path covered?)
  (define (is-covered? e)
    ;; we don't need to look at the span because the coverage is expression based
    (define p (syntax-position e))
    (if p
        (covered? p #:byte? #t)
        'missing))

  (define e
    (with-module-reading-parameterization
        (thunk (with-input-from-file path read-syntax))))
  (define (ret e) (values (e->n e) (a->n e)))
  (define (a->n e)
    (case (is-covered? e)
      [(covered uncovered) 1]
      [else 0]))
  (define (e->n e) (if (eq? (is-covered? e) 'covered) 1 0))
  (define-values (covered total)
    (let recur ([e e])
      (syntax-parse e
        [(v ...)
         (for/fold ([covered (e->n e)] [count (a->n e)])
                   ([e (in-syntax e)])
           (define-values (cov cnt) (recur e))
           (values (+ covered cov)
                   (+ count cnt)))]
        [e:expr (ret #'e)]
        [_ (values 0 0)])))
  (list covered total))
