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
      (with-module-reading-parameterization
          (thunk (read-syntax p (open-input-file p)))))
    (define-values (name anned)
      (syntax-parse (expand stx)
        #:datum-literals (module)
        [(~and s (module name:id lang forms ...))
         (values (syntax-e #'name)
                 (annotate-top #'s (namespace-base-phase ns)))]))
    (eval-syntax anned ns)
    ;; TODO run test/given submodule
    (parameterize ([current-namespace ns])
      (namespace-require `',name)))
  coverage)

(define (clear-coverage!)
  (dict-clear! coverage)
  (set! ns (make-base-empty-namespace))
  (namespace-attach-module (current-namespace) "coverage.rkt" ns))

(define (test-coverage-annotations)
  ;; can-annotate : (listof (list boolean srcloc))
  ;; boolean is #t => code was run
  ;;            #f => code was not run
  ;; remove those that cannot be annotated
  (define can-annotate
    (filter values
            (for/list ([(stx covered?) coverage])
              (and (syntax? stx)
                   (let ([src (syntax-source stx)]
                         [pos (syntax-position stx)]
                         [span (syntax-span stx)])
                     (and pos
                          span
                          #;
                          (hash-ref! port-name-cache src
                                     (λ () (send (get-defs) port-name-matches? src)))
                          (list covered?
                                (make-srcloc src #f #f pos span))))))))

  ;; filtered : (listof (list boolean srcloc))
  ;; remove redundant expressions
  (define filtered
    ;; actions-ht : (list src number number) -> (list boolean syntax)
    (let ([actions-ht (make-hash)])
      (for-each
       (λ (pr)
         (let* ([on? (list-ref pr 0)]
                [key (list-ref pr 1)]
                [old (hash-ref actions-ht key 'nothing)])
           (cond
            [(eq? old 'nothing) (hash-set! actions-ht key on?)]
            [old ;; recorded as executed
             (void)]
            [(not old) ;; recorded as unexected
             (when on?
               (hash-set! actions-ht key #t))])))
       can-annotate)
      (hash-map actions-ht (λ (k v) (list v k)))))
  filtered)
