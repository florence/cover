#lang racket/base
(require rackunit cover racket/runtime-path racket/port)
(define-runtime-path dont-run.rkt "dont-run.rkt")
(check-not-exn
 (lambda ()
   (check-true (test-files! (path->string dont-run.rkt)))))
