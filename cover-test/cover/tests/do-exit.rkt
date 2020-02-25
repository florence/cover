#lang racket/base
(require cover racket/runtime-path)
(define-runtime-path exit.rkt "exit.rkt")
(parameterize ([current-cover-environment (make-cover-environment)])
  (test-files! exit.rkt))
