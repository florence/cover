#lang racket/base
(provide coverage/c file-coverage/c coverage-gen/c)
(require racket/contract)

(define file-coverage/c (listof (list/c boolean? srcloc?)))
(define coverage/c (hash/c (and/c path-string? absolute-path?)
                           file-coverage/c))
(define coverage-gen/c (->* (coverage/c) (path-string?) any))
