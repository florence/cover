#lang racket
(provide generate-html-coverage)
(require syntax/modread 
         syntax/parse
         unstable/sequence 
         (only-in xml write-xexpr))
(module+ test (require rackunit "main.rkt"))


;;;;; main

;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage [dir "coverage"])
  (make-directory* dir)
  (for ([(k v) coverage])
    (define relative-file-name (string-replace k (path->string (build-path (current-directory))) ""))
    (define coverage-path (path->string (build-path (current-directory) dir)))
    (define coverage-file-relative (string-replace (string-replace relative-file-name ".rkt" "") "/" "-"))
    (define output-file (string-append coverage-path "/" coverage-file-relative ".html"))
    (with-output-to-file output-file
      (λ () (write-xexpr (make-html-file (hash-ref coverage k) relative-file-name))))))

;;;;; a Coverage is the output of (get-test-coverage)
;;;;; a FileCoverage is the values of the hashmap from (get-test-coverage)

;;;;; percentage
;; A Percentage is a [HashMap Type Real∈[0,1]]
;; a Type is one of: (update this as needed)
;; 'expr

;;  TODO this needs not count submodules and test directories

;; Coverage -> Percentage
(define (get-percentages/top coverage)
  (hash
   'expr (file-percentages->top expr-percentage coverage)))

(define (file-percentages->top get-% coverage)
  (define per-file
    (for/list ([(f v) coverage])
      (call-with-values (thunk (get-% f v)) list)))
  (define total (for/sum ([v per-file]) (second v)))
  (for/sum ([v per-file])
    (* (first v) (/ (second v) total))))

;; PathString FileCoverage -> Percentage
(define (get-percentages/file path coverage)
  (hash
   'expr (first (call-with-values (thunk (expr-percentage path coverage)) list))))

;;; percentage generators. each one has the type:
;; FileCoverage -> Real∈[0,1] Natural
;; there the Real is the percentage covered
;; and the Natural is the number of things of that type in the file

(define (expr-percentage path coverage)
  (define (is-covered? e)
    ;; we don't need to look at the span because the coverage is expression based
    (define p (syntax-position e))
    (covered? p coverage))

  (define e
    (with-module-reading-parameterization
        (thunk (with-input-from-file path
                 (thunk (read-syntax))))))
  (define (ret e)
    (values (e->n e) 1))
  (define (e->n e)
    (if (is-covered? e) 1 0))
  (define-values (covered count)
    (let recur ([e e])
      (syntax-parse e
        [x:id (ret #'x)]
        [(v ...)
         (for/fold ([covered (e->n e)] [count 1])
                   ([e (in-syntax e)])
           (define-values (cov cnt) (recur e))
           (define add (e->n e))
           (values (+ covered cov add)
                   (+ count cnt 1)))]
        [e:expr (ret #'e)]
        [_ (values 0 0)])))
  (values (/ covered count) count))

(module+ test
  (test-begin
   (define f (path->string (build-path (current-directory) "tests/basic/prog.rkt")))
   (test-files! f)
   (define-values (result _) (expr-percentage f (hash-ref (get-test-coverage) f)))
   (check-equal? result 1)
   (clear-coverage!)))

;;;;; html
;; FileCoverage PathString -> Xexpr
(define (make-html-file coverage path)
  (define %age (get-percentages/file path coverage))
  `(html ()
    (body ()
          ,@(for/list ([(type %) %age])
              `(p () ,(~a type ': " " (~r (* 100 %) #:precision 2) "%") (br ())))
          ,@(file->html coverage path))))

(module+ test
  (test-begin
   (define f
     (path->string (build-path (current-directory) "tests/basic/prog.rkt")))
   (test-files! f)
   (check-equal? (make-html-file (hash-ref (get-test-coverage) f) f)
                 `(html ()
                   (body ()
                         (p () "expr: 100%" (br ()))
                         ,@(file->html (hash-ref (get-test-coverage) f) f))))
   (clear-coverage!)))

(define (file->html cover path)
  (define file (file->string path))
  (let loop ([loc 1] [start 1] [left (string-length file)] [mode (covered? 1 cover)])
    (define (get-xml)
      (mode-xml mode (encode-string (substring file (sub1 start) (sub1 loc)))))
    (case left
      [(0) (list (get-xml))]
      [else
       (define m (covered? loc cover))
       (define (loop* start) (loop (add1 loc) start (sub1 left) m))
       (if (eq? m mode)
           (loop* start)
           (cons (get-xml)
                 (loop* (add1 loc))))])))

(define (get-mode loc c)
  (define-values (mode _)
    (for/fold ([mode 'none] [last-start 0])
              ([pair c])
      (match pair
        [(list m (srcloc _ _ _ start range))
         (if (and (<= start loc (+ start range))
                  (or (eq? mode 'none)
                      (> start last-start)))
             (values m start)
             (values mode last-start))])))
  mode)

(define (encode-string c)
  (foldr (λ (el rst) (cons el (cons '(br ()) rst)))
         '()
         (string-split c "\n")))

(define (mode-xml mode body)
  (define color (if mode "green" "red"))
  `(div ((style ,(string-append "color:" color))) ,@body))

(module+ test
  (define (test f out)
    (define file (path->string (build-path (current-directory) f)))
    (test-files! file)
    (check-equal? (file->html (hash-ref (get-test-coverage) file)
                              file)
                  out)
    (clear-coverage!))
  (test "tests/basic/prog.rkt"
        `((div ((style "color:green"))
          ,@(encode-string (file->string "tests/basic/prog.rkt"))))))


;;;; utils

;; Natural FileCoverage -> Boolean
(define (covered? loc c)
  (define-values (mode _)
    (for/fold ([mode 'none] [last-start 0])
              ([pair c])
      (match pair
        [(list m (srcloc _ _ _ start range))
         (if (and (<= start loc (+ start range))
                  (or (eq? mode 'none)
                      (> start last-start)))
             (values m start)
             (values mode last-start))])))
  (if (boolean? mode)
      mode
      (error "loc ~s not in coverage" loc)))
