#lang racket/base
(provide test-files clear-coverage!)
(require racket/dict
         racket/function
         syntax/modread
         syntax/parse
         "coverage.rkt"
         "strace.rkt")


(define ns (make-base-empty-namespace))
(namespace-attach-module (current-namespace) "coverage.rkt" ns)

(define (test-files . paths)
  (for ([p paths])
    (define stx 
      (with-module-reading-parameterization (thunk (read-syntax p (open-input-file p)))))
    (define-values (name anned) 
      (syntax-parse (expand stx)
        #:datum-literals (module)
        [(~and s (module name:id lang forms ...))
         (values (syntax-e #'name)
                 (annotate-top #'s (namespace-base-phase ns)))]))
    (eval-syntax anned ns)
    (parameterize ([current-namespace ns])
      (namespace-require `',name)))
  coverage)

(define (clear-coverage!)
  (dict-clear! coverage)
  (set! ns (make-base-empty-namespace))
  (namespace-attach-module (current-namespace) "coverage.rkt" ns))
