#lang racket
(require "../cover.rkt" racket/runtime-path rackunit)
(define-runtime-path arg.rkt "arg.rkt")
(check-true (test-files! (list (path->string arg.rkt) #("a"))))
