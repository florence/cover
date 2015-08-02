#lang racket/base
(require rackunit)
(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))
(check-equal? (eval `(+ 1 1) ns) 2)
