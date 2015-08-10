#lang racket
(require rackunit cover racket/runtime-path)
(define-runtime-path prog "prog.rkt")
(test-case
 "A new non-parented logger should not cause a hang"
 (define t
   (thread
    (lambda ()
      (parameterize ([current-logger (make-logger)])
        (test-files! prog)))))
 (define r (sync/timeout 5 t))
 (check-not-false (and r (thread-dead? r))))
