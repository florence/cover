#lang racket
(provide generate-html-coverage)
(require racket/runtime-path
         syntax/modread
         syntax/parse
         unstable/sequence
         (only-in xml write-xexpr)
         "format-utils.rkt"
         "shared.rkt")


(module+ test
  (require rackunit "../cover.rkt" racket/runtime-path)
  (define-runtime-path tests/basic/prog.rkt "../tests/basic/prog.rkt"))

;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage [d "coverage"])
  (define dir (simplify-path d))
  (make-directory* dir)
  (define file-list
    (for/list ([(k v) coverage])
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
      (make-directory* output-dir)
      (with-output-to-file output-file
        (λ ()
          (define expr (make-html-file (hash-ref coverage k) k path-to-css))
          (vprintf "writing html coverage for ~s to ~s\n" k output-file)
          (write-xexpr expr))
        #:exists 'replace)
      output-file))
  (define index (build-index coverage file-list))
  (with-output-to-file (build-path dir "index.html")
    #:exists 'replace
    (thunk (write-xexpr index)))
  (move-support-files! dir))

(define (build-index coverage file-list)
  (vprintf "building index.html\n")
  (define %ages (get-percentages/top coverage))
  `(html
    (head ()
          (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
    (body
     ,(%s->xexpr %ages)
     (div ()
          ,@(for/list ([file file-list])
              (define f (path->string (apply build-path (rest (explode-path file)))))
              `(p () (a ([href ,f]) ,f)))))))

(module+ test
  (define-runtime-path prog.rkt "../tests/basic/prog.rkt")
  (test-begin
   (after
    (test-files! (path->string (simplify-path prog.rkt)))
    (define coverage (get-test-coverage))
    (check-equal?
     (build-index coverage '("./tests/basic/prog.rkt"))
     `(html (head () (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
       (body ,(%s->xexpr 1)
             (div () (p () (a ([href "tests/basic/prog.rkt"]) "tests/basic/prog.rkt"))))))
    (clear-coverage!))))

(define-runtime-path css "main.css")
(define (move-support-files! dir)
  (copy-file css (build-path dir "main.css") #t))

;; FileCoverage PathString PathString -> Xexpr
(define (make-html-file coverage path path-to-css)
  (define covered? (make-covered? coverage path))
  (define %age (get-percentages/file path covered?))
  `(html ()
    (head ()
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [type "text/css"] [href ,path-to-css])))
    (body ()
          ,(%s->xexpr %age)
          (div ([class "code"])
               ,(file->html path covered?)))))

(define (%s->xexpr %)
  `(p () ,(~a "expr" ': " " (~r (* 100 %) #:precision 2) "%") (br ())))

(module+ test
  (define-runtime-path path "../tests/basic/prog.rkt")
  (test-begin
   (define f (path->string (simplify-path path)))
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
      [(yes) "covered"]
      [(no) "uncovered"]
      [(missing) "missing"]))
  `(span ((class ,class)) ,body))

(module+ test
  (define (test file out)
    (test-files! file)
    (define cov (hash-ref (get-test-coverage) file))
    (define covered? (make-covered? cov file))
    (check-equal? (file->html file covered?)
                  out)
    (clear-coverage!))
  (define f (path->string (simplify-path path)))
  (test f
        `(ol ()
          (li ()
              ,@(for/list ([c (first (string-split (file->string f) "\n"))])
                 `(span ((class "covered"))
                   ,(encode-char c))))
          ,@(for/list ([l (rest (string-split (file->string f) "\n"))])
              `(li ()
                ,@(for/list ([c l])
                    `(span ((class ,(if (equal? c #\space) "missing" "covered")))
                      ,(encode-char c))))))))


;;;;; percentage
;; A Percentage is a Real∈[0,1]
;; a Type is one of: (update this as needed)
;; 'expr

;; Coverage -> Percentage
(define (get-percentages/top coverage)
  (file-percentages->top expr-percentage coverage))
(module+ test
  (test-begin
   (after
    (test-files! (path->string (simplify-path tests/basic/prog.rkt)))
    (check-equal? (get-percentages/top (get-test-coverage)) 1)
    (clear-coverage!))))

(define (file-percentages->top get-% coverage)
  (define per-file
    (for/list ([(f v) coverage])
      (define covered? (make-covered? v f))
      (call-with-values (thunk (get-% f covered?)) list)))
  (define total (for/sum ([v per-file]) (second v)))
  (for/sum ([v per-file])
    (* (first v) (/ (second v) total))))

;; PathString Covered? -> Percentage
(define (get-percentages/file path covered?)
  (first (call-with-values (thunk (expr-percentage path covered?)) list)))

;;; percentage generators. each one has the type:
;; FilePath Covered? -> Real∈[0,1] Natural
;; there the Real is the percentage covered
;; and the Natural is the number of things of that type in the file
(define (expr-percentage path covered?)
  (define (is-covered? e)
    ;; we don't need to look at the span because the coverage is expression based
    (define p (syntax-position e))
    (covered? p #:byte? #t))

  (define e
    (with-module-reading-parameterization
        (thunk (with-input-from-file path read-syntax))))
  (define (ret e)
    (values (e->n e) (a->n e)))
  (define (a->n e)
    (case (is-covered? e)
      [(yes no) 1]
      [else 0]))
  (define (e->n e)
    (if (eq? (is-covered? e) 'yes) 1 0))
  (define-values (covered count)
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
  (values (/ covered count) count))

(module+ test
  (test-begin
   (define f (path->string (simplify-path tests/basic/prog.rkt)))
   (test-files! f)
   (define covered? (make-covered? (hash-ref (get-test-coverage) f) f))
   (define-values (result _) (expr-percentage f covered?))
   (check-equal? result 1)
   (clear-coverage!)))
