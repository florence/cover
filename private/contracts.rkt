#lang racket/base
(provide coverage/c file-coverage/c coverage-gen/c)
(require racket/contract)

(define file-coverage/c (listof (list/c boolean? srcloc?)))
;; if its a file path, will be an absolute path
(define coverage/c (hash/c any/c file-coverage/c))
(define coverage-gen/c (->* (coverage/c) (path-string?) any))
