#lang racket/base
(require "../main.rkt" racket/runtime-path)
(define-runtime-path multiple-modules.rkt "multiple-modules.rkt")
(parameterize ([current-cover-environment (make-cover-environment)])
  (test-files! multiple-modules.rkt #:submod '(test))
  (test-files! multiple-modules.rkt #:submod '(a))
  (test-files! multiple-modules.rkt #:submod '(test a b)))

