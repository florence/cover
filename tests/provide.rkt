#lang racket/base
(provide test
         (struct-out tt))
(define test 5)
(struct tt (a b c) #:transparent)
