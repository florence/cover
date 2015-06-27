#lang racket/base
(require "../main.rkt" racket/runtime-path)
(define-runtime-path exit.rkt "exit.rkt")
(parameterize ([current-cover-environment (make-cover-environment)])
  (test-files! exit.rkt))
