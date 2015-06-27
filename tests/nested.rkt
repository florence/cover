#lang racket
(require rackunit cover racket/runtime-path
         "../private/file-utils.rkt")

(define-runtime-path prog "basic/prog.rkt")
(define-runtime-path cov "../cover.rkt")
(define-runtime-path other "simple-multi/2.rkt")

(define (do-test . files)
  (define key (->absolute (first files)))
  (void
   (parameterize ([current-cover-environment (make-cover-environment)])
     (check-true (apply test-files! files))
     (parameterize ([current-cover-environment (make-cover-environment)])
       (parameterize ([current-cover-environment (make-cover-environment)])
         (check-true (apply test-files! files))
         ((get-test-coverage) key 1))
       (check-true (apply test-files! files))
       ((get-test-coverage) key 1))
     ((get-test-coverage) key 1))))


;; these tests are logically "check-not-exn"s but that obsures inner test failures
(test-case
 "Prog nested coverage"
 (do-test prog))

(test-case
 "Cover nested coverage"
 (do-test cov))

(test-case
 "Cover nested coverage with many files"
 (do-test cov other))
