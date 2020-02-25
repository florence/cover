#lang racket/base
(provide coverage/c coverage-gen/c)
(require racket/contract)

;; if its a file path, will be an absolute path
(define coverage/c (-> any/c exact-positive-integer?
                       (or/c 'covered 'uncovered 'irrelevant)))
(define coverage-gen/c (->* (coverage/c (listof path-string?)) (path-string?) any))
