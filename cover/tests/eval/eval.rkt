#lang racket
(require (for-syntax "eval-call.rkt"))
(begin-for-syntax
  (eval-the-thing #'(module x racket 0)))
