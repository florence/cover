#lang racket
(require syntax/modread syntax/parse unstable/sequence)
(module+ test (require rackunit "main.rkt"))

;;;;; a Coverage is the output of (get-test-coverage)
;;;;; a FileCoverage is the values of the hashmap from (get-test-coverage)

;;;;; percentage
;; A Percentage is a [HashMap Type Real∈[0,1]]
;; a Type is one of: (update this as needed)
;; 'expr

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

;; PathString (list (list bool srcloc)) ->  Coverage
(define (get-percentages/file path coverage)
  (hash
   'expr (expr-percentage path coverage)))

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
   (define f "tests/basic/prog.rkt")
   (test-files! f)
   (define-values (result _) (expr-percentage f (hash-ref (get-test-coverage) f)))
   (check-equal? result 1)
   (clear-coverage!)))

;;;;; html
(define (make-html-file coverage path)
  (string-append
   "<html><body>"
   (file->html coverage path)
   "</body></html>"))
(define (file->html coverage paths)
  (for/list ([path paths])
    (define file (file->string path))
    (define cover (hash-ref coverage path))
    (define data
      (let loop ([loc 1] [chars (string->list file)] [mode 'none])
        (match chars
          [(list) (mode->end mode)]
          [(cons c r)
           (define (loop* me) (loop (add1 loc) r m))
           (define m (covered? loc cover))
           (define encoded (encode-char c))
           (if (eq? m mode)
               (cons encoded (loop* mode))
               (append (mode->end mode)
                       (mode->start m)
                       (list encoded)
                       (loop* m)))])))
    (apply string data)))

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

(define (encode-char c) c)

(define covered-mode-start "<span style=\"color:green\">")
(define uncovered-mode-start "<span style=\"color:red\">")
(define (mode->start mode)
  (string->list
   (match mode
     ['none ""]
     [#t covered-mode-start]
     [#f uncovered-mode-start])))

(define mode-end "</span>")
(define (mode->end mode)
  (string->list
   (match mode
     ['none ""]
     [_ mode-end])))

(module+ test
  (define (test file out)
    (test-files! file)
    (check-equal? (first (file->html (get-test-coverage) (list file)))
                  out)
    (clear-coverage!))
  (test "tests/basic/prog.rkt"
        (string-append covered-mode-start
                       (apply string
                              (map encode-char
                                   (string->list (file->string "tests/basic/prog.rkt"))))
                       mode-end)))


;;;; utils

;; FileCoverage -> Boolean
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
