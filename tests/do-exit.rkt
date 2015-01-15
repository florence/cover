#lang racket/base
(require "../main.rkt" racket/runtime-path)
(define-runtime-path exit.rkt "exit.rkt")
(test-files! exit.rkt)
(clear-coverage!)
