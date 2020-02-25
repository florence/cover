#lang racket
(provide eval-the-thing)
(define (eval-the-thing stx)
  (eval #`(expand #'#,stx) (make-base-namespace)))
