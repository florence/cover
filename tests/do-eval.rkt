#lang racket/base
(require racket/runtime-path "../main.rkt" rackunit)
(define-runtime-path eval.rkt "eval.rkt")
(check-true (test-files! eval.rkt))
(clear-coverage!)
