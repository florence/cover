#lang racket/base
(provide generate-html-coverage)
(require racket/file
         racket/path
         racket/math
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
  (define-runtime-path tests/basic/prog.rkt "../tests/basic/prog.rkt")
  (define (mock-covered? pos) 
    (cond [(<= 1 pos 6) 'covered]
          [(= 6 pos) 'missing]
          [else 'uncovered])))

;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage [d "coverage"])
  (define dir (simplify-path d))
  (define fs (get-files coverage dir))
  (write-files fs)
  (move-support-files! dir))

(define (get-files coverage dir)
  (define file-list
    (for/list ([(k v) (in-hash coverage)])
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
  (for ([l (in-list f)])
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
  (define lines (string-split file "\n"))
  `(div ()
        ,(div:line-numbers (length lines))
        ,(div:file-lines lines covered?)))

(module+ test
  (test-begin
   (define f (path->string (simplify-path tests/basic/prog.rkt)))
   (test-files! f)
   (define cov (hash-ref (get-test-coverage) f))
   (define covered? (make-covered? cov f))
   (define lines (string-split (file->string f) "\n"))
   (check-equal? (file->html f covered?) 
                 `(div ()
                       ,(div:line-numbers (length lines))
                       ,(div:file-lines lines covered?)))
   (clear-coverage!)))

;; File Report
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nat -> Xexpr
;; create a div with line numbers in it
(define (div:line-numbers line-count)
  `(div ([class "line-numbers"]) 
        ,@(for/list ([num (in-range 1 (add1 line-count))])
            `(div () ,(number->string num)))))

(module+ test
  (check-equal? 
   (div:line-numbers 5)
   `(div ([class "line-numbers"]) 
         ,@(build-list 5 (λ (n) `(div () ,(number->string (add1 n))))))))

;; [List String] Covered? -> Xexpr
(define (div:file-lines file-lines covered?)
  (define-values (line-divs _) 
    (for/fold ([lines '()] [pos 1]) ([line (in-list file-lines)])
      (values (cons (div:file-line line pos covered?) lines)
              (add1 (+ pos (string-length line))))))
  `(div ([class "file-lines"]) ,@(reverse line-divs)))

(module+ test
  (define lines '("hello world" "goodbye"))
  (check-equal? (div:file-lines lines mock-covered?)
                `(div ([class "file-lines"])
                      ,(div:file-line (first lines) 1 mock-covered?)
                      ,(div:file-line (second lines) 12 mock-covered?))))

;; String Nat Covered? -> Xexpr
;; Build a single line into an Xexpr
(define (div:file-line line pos covered?)
  (cond [(zero? (string-length line)) '(br ())]
        [else 
         (define (build-span str type) `(span ([class ,(symbol->string type)]) ,str))
         (define (add-expr cover-type expr cover-exprs)
           (if cover-type 
               (cons (build-span expr cover-type) cover-exprs)
               cover-exprs))
         
         (define-values (xexpr acc/str coverage-type)
           (for/fold ([covered-exp '()] [expr/acc ""] [current-cover-type #f])
                     ([c (in-string line)] [offset (in-naturals)])
             (cond [(equal? c #\space) 
                    (define new-expr (cons 'nbsp (add-expr current-cover-type expr/acc covered-exp)))
                    (values new-expr "" #f)]
                   [(equal? current-cover-type (covered? (+ pos offset)))
                    (values covered-exp (string-append expr/acc (string c)) current-cover-type)]
                   [else 
                    (define new-expr (add-expr current-cover-type expr/acc covered-exp))
                    (values new-expr (string c) (covered? (+ pos offset)))])))
         (define result 
           (if coverage-type 
               (cons (build-span acc/str coverage-type) xexpr)
               xexpr))
         `(div ([class "line"]) ,@(reverse result))]))

(module+ test
  (check-equal? (div:file-line "" 1 mock-covered?) '(br ()))
  (check-equal? (div:file-line "hello world" 1 mock-covered?)
                '(div ([class "line"]) (span ([class "covered"]) "hello")
                      nbsp
                      (span ([class "uncovered"]) "world"))))

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
               ,(table:file-reports expression-coverage)))))

;; [Hash FilePath ExpressionInfo] -> Xexpr
(define (div:total-coverage expr-coverages)
  (define total-coverage-percentage (expression-coverage-percentage/all expr-coverages))
  (define coverage-as-string
    (if (equal? +nan.0 total-coverage-percentage)
        "No Coverage Information"
        (real->decimal-string total-coverage-percentage)))
  `(div ([class "total-coverage"])
        ,(string-append "Total Project Coverage: " coverage-as-string "%")))

(module+ test 
  (test-begin (check-equal? (div:total-coverage (hash "foo.rkt" (list 0 10)
                                                      "bar.rkt" (list 10 10)))
                            '(div ([class "total-coverage"]) "Total Project Coverage: 50.00%"))))

;; [Hash FilePath ExpressionInfo] -> Xexpr
(define (table:file-reports expr-coverages)
  `(table ([class "file-list"])
          (thead ()
                 (tr ()
                     (th ([class "file-name"]) "File")
                     (th () "Coverage Percentage")
                     (th () "Covered Expressions")
                     (th () "Total Expressions")))
          (tbody ()
                 ,@(for/list ([(path expr-info) (in-hash expr-coverages)] [line-num (in-naturals)])
                     (tr:file-report path expr-info (zero? (modulo line-num 2)))))))

;; PathString ExpressionInfo Boolean -> Xexpr
;; create a div that holds a link to the file report and expression
;; coverage information
(define (tr:file-report path expr-coverage-info stripe?)
  (define local-file 
    (path->string (find-relative-path (current-directory) (string->path path))))
  (define percentage 
    (cond
      [(zero? (second expr-coverage-info)) +nan.0]
      [else (exact->inexact (* 100 (/ (first expr-coverage-info) 
                                       (second expr-coverage-info))))]))
  (define styles `([class ,(string-append "file-info" (if stripe? " stripe" ""))]))
  `(tr ,styles
        (td ([class "file-name"]) (a ([href ,(coverage-report-link path)]) ,local-file))
        (td () ,(if (nan? percentage) "No Coverage Info" (real->decimal-string percentage)))
        (td () ,(real->decimal-string (first expr-coverage-info)))
        (td () ,(real->decimal-string (second expr-coverage-info)))))

(module+ test
  (test-begin (check-equal? (tr:file-report "foo.rkt" (list 0 0) #f)
                            '(tr ((class "file-info"))
                                  (td ([class "file-name"]) (a ((href "foo.html")) "foo.rkt"))
                                  (td () "No Coverage Info")
                                  (td () "0.00")
                                  (td () "0.00"))))
  (test-begin (check-equal? (tr:file-report "foo.rkt" (list 10 10) #t)
                            '(tr ((class "file-info stripe"))
                                  (td ([class "file-name"]) (a ((href "foo.html")) "foo.rkt"))
                                  (td () "100.00")
                                  (td () "10.00")
                                  (td () "10.00")))))

;; Path -> String
;; Generate a link to the coverage report
(define (coverage-report-link path)
  (define local-file (find-relative-path (current-directory) path))
  (path->string (path-replace-suffix local-file ".html")))

(module+ test
  (test-begin
   (check-equal? (coverage-report-link "format-utils.rkt")
                 "format-utils.html")))

;; Percentage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Percentage is a Real∈[0,100]

;; [Hash FilePath ExpressionInfo] -> Percentage
;; Get the total expression conversion percentage for the whole project
(define (expression-coverage-percentage/all all-expr-info)
  (define total-covered (for/sum ([v (in-list (hash-values all-expr-info))]) (first v)))
  (define total-exprs (for/sum ([v (in-list (hash-values all-expr-info))]) (second v)))
  (if (zero? total-exprs)
      +nan.0
      (* (/ total-covered total-exprs) 100)))

(module+ test
  (test-begin 
   (check-equal? 
    (expression-coverage-percentage/all (hash "foo.rkt" (list 0 10)
                                              "bar.rkt" (list 10 10)))
    50))
  
  (test-begin (check-equal? (expression-coverage-percentage/all (hash "foo.rkt" (list 0 0)))
                            +nan.0)))

;; Expression Coverage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ExpressionInfo is a (List Nat Nat) where:
;; the first element is the number of covered expressions
;; the second element is the total number of expressions

;; Coverage -> [Hash FilePath ExpressionInfo]
;; returns a hash that maps file paths to an ExpressionInfo
(define (expression-coverage/all coverage)
  (for/hash ([(file data) (in-hash coverage)])
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
