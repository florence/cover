#lang racket/base
(require rackunit syntax/strip-context
         version/utils
         (for-syntax racket/base
                     racket/syntax))
(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))
(define (test ns)
  (parameterize ([current-namespace ns])
    (namespace-require '(for-syntax (only racket/base + eval quote #%app #%datum)))
    (check-equal? (eval '(+ 1 1) ) 2)
    (check-not-exn (lambda () (eval '(begin-for-syntax (+ 1 1)))))

    (check-equal? (eval (quote-syntax (+ 1 1)) ) 2)
    (check-not-exn (lambda () (eval (quote-syntax (begin-for-syntax (+ 1 1))))))
    (check-equal? (eval (strip-context (quote-syntax (+ 1 1)))) 2)
    (check-not-exn (lambda () (eval (strip-context (quote-syntax (begin-for-syntax (+ 1 1)))))))
    ;; it looks like the old C expander doesn't pass the namespace along properly,
    ;; or something to that effect.
    (unless (version<=? (version) "7.0")
      (check-not-exn (lambda () (eval '(begin-for-syntax (eval '(+ 1 1))))))
      (check-not-exn (lambda () (eval (quote-syntax (begin-for-syntax (eval '(+ 1 1)))))))
      (check-not-exn (lambda () (eval (strip-context (quote-syntax (begin-for-syntax (eval '(+ 1 1)))))))))))

(test ns)
(test (make-base-namespace))


(define-syntax (test-phase-one-eval stx)
  (syntax-case stx ()
    [(_ body)
     #`'#,(list (syntax-local-eval #'body) (eval #'body))]))

(check-equal? (test-phase-one-eval (+ 1 1)) (list 2 2))