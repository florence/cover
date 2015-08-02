#lang racket
;; These tests modified from https://github.com/jackfirth/point-free
(provide define/compose
         arg-count
         define/arg-count)

(define-syntax-rule (define/compose id f ...)
  (define id (compose f ...)))

(define-syntax-rule (arg-count n expr)
  (lambda args
    (let ([n (length args)])
      (apply expr args))))

(define-syntax-rule (define/arg-count id n expr)
  (define id (arg-count n expr)))

(module+ test
  (require rackunit)
  (define-binary-check (check-syntax-datum stx-actual stx-expected)
    (equal? (syntax->datum stx-actual)
            (syntax->datum stx-expected)))
  (check-syntax-datum (expand-once #'(arg-count n identity))
                      #'(lambda args (let ([n (length args)]) (apply identity args))))
  (check-syntax-datum (expand-once #'(define/arg-count num-args n identity))
                      #'(define num-args (arg-count n identity)))
  (define num-args1
    (arg-count n (const n)))
  (check-eqv? (num-args1 0 0) 2)
  (define/arg-count num-args2 n (const n))
  (check-eqv? (num-args2 'foo 'bar 'baz) 3))
