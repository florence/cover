#lang racket
;; takes all diretories given in the test submodule
;; for every .rkt file in those directories it loads
;; tests that file and checks its coverage against an
;; .rktl file of the same name
(require (only-in "../main.rkt" test-files! clear-coverage! get-test-coverage irrelevant-submodules
                  make-covered?)
         "../private/file-utils.rkt"
         racket/runtime-path rackunit)

(define (test-dir d)
  (define files
    (for*/list ([p (directory-list d)]
                [s (in-value (path->string (build-path d p)))]
                #:when (regexp-match #rx"\\.rkt$" s))
      s))
  (define covered
    (for/hash ([f files])
      (values f
              (path->string (path-replace-suffix f ".rktl")))))

  (define (do-test files)
    (apply test-files! files)

    (define coverage (get-test-coverage))
    (for ([(program cover) covered])
      (define actual-coverage (hash-ref coverage program))
      (define-values (expected-coverage expected-uncoverage)
        (with-input-from-file cover (lambda () (values (ranges->numbers (read))
                                                       (ranges->numbers (read))))))
      (define covered? (make-covered? program actual-coverage))
      (test-begin
       (for ([i expected-coverage])
         (check-equal? (covered? i) 'covered
                       (format "expected char ~a to be covered, but it was not, in: ~s"
                               i program)))
       (for ([i expected-uncoverage])
         (check-equal? (covered? i) 'uncovered
                       (format "expected char ~a to be uncovered, but it was, in: ~s"
                               i program)))))

    (clear-coverage!))

  ;; ensure the results are the same regardless of file order
  (do-test files)
  (do-test (reverse files)))

(define (ranges->numbers range)
  (match range
    [(list) null]
    [(cons (list a b) r)
     (if (equal? a b)
         (ranges->numbers r)
         (cons a (ranges->numbers (cons (list (add1 a) b) r))))]))

(module+ test
  (define-runtime-path-list test-dirs '("basic" "simple-multi" "syntax" "at-exp"))
  (for-each (compose test-dir path->string) test-dirs)
  (define-runtime-path submods "submods")
  (parameterize ([irrelevant-submodules null])
    (test-dir (path->string submods))))

(module+ test
  (define-runtime-path prog.rkt "prog.rkt")
  (test-begin
   (after
    (test-files! (->absolute prog.rkt))
    (define abs (get-test-coverage))
    (test-files! (build-path (->relative prog.rkt)))
    (define rel (get-test-coverage))
    (check-equal? abs rel)
    (clear-coverage!))))
