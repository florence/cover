#lang racket/base
(provide test-files clear-coverage!)
(require racket/dict
         racket/function
         syntax/modread
         "coverage.rkt"
         "strace.rkt")


(define ns (make-base-empty-namespace))
(namespace-attach-module (current-namespace) "coverage.rkt" ns)

(define (test-files . paths)
  (for ([p paths])
    (define stx 
      (with-module-reading-parameterization (thunk (read-syntax p (open-input-file p)))))
    (define anned (annotate-top (expand stx) (namespace-base-phase ns)))
    (eval-syntax anned ns))
  coverage)

(define (clear-coverage!)
  (dict-clear! coverage)
  (set! ns (make-base-empty-namespace))
  (namespace-attach-module (current-namespace) "coverage.rkt" ns))
