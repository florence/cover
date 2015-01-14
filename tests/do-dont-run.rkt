#lang racket/base
(require rackunit "../main.rkt" racket/runtime-path)
(define-runtime-path dont-run.rkt "dont-run.rkt")
(check-not-exn
 (lambda () (test-files! (path->string dont-run.rkt))))
