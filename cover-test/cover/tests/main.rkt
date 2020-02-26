#lang racket
;; takes all diretories given in the test submodule
;; for every .rkt file in those directories it loads
;; tests that file and checks its coverage against an
;; .rktl file of the same name
;; also checks that no warnings are logged during coverage (specifically to check that
;; zero-width id's are valid)
(require (only-in cover test-files! get-test-coverage irrelevant-submodules
                  current-cover-environment make-cover-environment)
         (only-in cover/cover coverage-wrapper-map)
         cover/private/file-utils
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
    (parameterize ([current-cover-environment (make-cover-environment)]
                   [port-count-lines-enabled (> 0.5 (random))]
                   [current-error-port (current-output-port)])
      (define reciever (make-log-receiver (current-logger) 'warning 'cover))
      
      (apply test-files! files)

      (define coverage (get-test-coverage))
      (for ([(program cover) covered])
        (define-values (expected-coverage expected-uncoverage)
          (with-input-from-file cover (lambda () (values (ranges->numbers (read))
                                                         (ranges->numbers (read))))))
        
        (define covered? (curry coverage program))
        (check-not-exn
         (lambda ()
           (covered? 1)))
        (define (test-range range type)
          (for ([i range])
            (define v (covered? i))
            (unless (eq? v 'irrelevant)
              (check-equal? v type
                            (format "expected char ~a to be covered, but it was not, in: ~s"
                                    i program)))))
        (test-begin
         (test-range expected-coverage 'covered)
         (test-range expected-uncoverage 'uncovered)))
      
      (check-false (sync/timeout (lambda () #f) reciever))))

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
  (require (for-syntax version/utils) version/utils)
  (define-runtime-path-list test-dirs (append  (list "basic" "simple-multi" "syntax" "at-exp"
                                                     "multibyte-coverage" "provide-define-syntax")))
  (for-each (compose test-dir path->string) test-dirs)
  (define-runtime-path submods "submods")
  (parameterize ([irrelevant-submodules null])
    (test-dir (path->string submods))))

(module+ test
  (define-runtime-path prog.rkt "prog.rkt")
  (test-begin
   (parameterize ([current-cover-environment (make-cover-environment)])
     (test-files! (->absolute prog.rkt))
     (define abs (coverage-wrapper-map (get-test-coverage)))
     (test-files! (build-path (->relative prog.rkt)))
     (define rel (coverage-wrapper-map (get-test-coverage)))
     (check-equal? abs rel))))
