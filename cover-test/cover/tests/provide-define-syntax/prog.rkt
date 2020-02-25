#lang racket
(require (for-syntax syntax/parse))
(define-syntax (define/broken-provide stx)
  (syntax-parse stx
    [(_ x:id y)
     #'(begin
         (define-syntax x (make-rename-transformer #'y))
         (provide x)
         (define (k) x))]))

(define/broken-provide ++ +)
