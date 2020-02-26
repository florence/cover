#lang racket/base
(require syntax/parse)
(define-syntax-class n
  [pattern x:number])
(syntax-parse #'()
  [((~optional :n #:defaults ([x #'1])))
   #'x])