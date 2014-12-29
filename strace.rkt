#lang racket/base
(provide annotate-top test-coverage-enabled)
(require errortrace/stacktrace
         racket/function
         racket/unit
         "coverage.rkt")

(define (with-mark src dest phase) dest)
(define test-coverage-enabled (make-parameter #t))

(define (initialize-test-coverage-point stx)
  (hash-set! coverage stx #f))
(define (test-covered stx)
  (thunk (hash-set! coverage stx #t)))

(define profile-key (gensym))

(define profiling-enabled (make-parameter #f))
(define initialize-profile-point void)
(define (register-profile-start . a) #f)
(define register-profile-done void)

(define-values/invoke-unit/infer stacktrace@)
