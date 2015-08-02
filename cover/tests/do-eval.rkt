#lang racket/base
(require racket/runtime-path "../main.rkt" rackunit)
(define-runtime-path eval.rkt "eval.rkt")
(parameterize ([current-cover-environment (make-cover-environment)])
  (check-true (test-files! eval.rkt)))
