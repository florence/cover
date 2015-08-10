#lang racket
(require cover rackunit racket/runtime-path cover/private/file-utils)
(define-runtime-path file "cross-phase-persist.rkt")
(test-case
 "covering cross-phase-persistent files should enter them into the coverage table"
 (parameterize ([current-cover-environment (make-cover-environment)])
   (test-files! file)
   (define c #f)
   (check-not-exn
    (lambda ()
      (set! c (get-test-coverage))))
   (check-not-exn
    (lambda ()
      (c (->absolute file) 1)))
   (check-equal? (c (->absolute file) 1) 'covered)))
