#lang racket
(require cover racket/runtime-path rackunit)
(define-runtime-path eval.rkt "eval.rkt")
(define-runtime-path eval-call.rkt "eval-call.rkt")
(check-not-exn (lambda () (test-files!
                           (path->string eval-call.rkt)
                           (path->string eval.rkt))))
