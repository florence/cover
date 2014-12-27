#lang racket/base
(provide test-files! clear-coverage! get-test-coverage)
(require (for-syntax racket/base))
(require racket/dict
         racket/function
         syntax/modread
         syntax/parse
         "coverage.rkt"
         "strace.rkt"
         racket/runtime-path)



(define ns (make-base-namespace))
(define-runtime-path cov "coverage.rkt")
(namespace-attach-module (current-namespace) cov ns)

(define-syntax (with-ns stx)
  (syntax-case stx ()
    [(_ b ...)
     #'(parameterize ([current-namespace ns])
         b ...)]))

;; PathString * -> Void
;; Test files and build coverage map
(define (test-files! . paths)
  (for ([p paths])
    (define stx
      (with-module-reading-parameterization
          (thunk (read-syntax p (open-input-file p)))))
    (define-values (name anned)
      (syntax-parse (with-ns (expand stx))
        #:datum-literals (module)
        [(~and s (module name:id lang forms ...))
         (values (syntax-e #'name)
                 (annotate-top #'s (namespace-base-phase ns)))]))
    (eval-syntax anned ns)
    ;; TODO run test/given submodule
    (with-ns (namespace-require `',name))))

;; -> Void
;; clear coverage map
(define (clear-coverage!)
  (dict-clear! coverage)
  (set! ns (make-base-namespace))
  (namespace-attach-module (current-namespace) cov ns))

;; -> [Hashof PathString (List Boolean srcloc)]
;; returns a hash of file to a list, where the first of the list is if
;; that srcloc was covered or not
;; based on <pkgs>/drracket/drracket/private/debug.rkt
(define (get-test-coverage)
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
                          (list covered?
                                (make-srcloc src #f #f pos span))))))))

  ;; actions-ht : (list src number number) -> (list boolean syntax)
  (define actions-ht (make-hash))

  (for-each
   (λ (pr)
     (let* ([on? (car pr)]
            [key (cadr pr)]
            [old (hash-ref actions-ht key 'nothing)])
       (cond
        [(eq? old 'nothing) (hash-set! actions-ht key on?)]
        [old ;; recorded as executed
         (void)]
        [(not old) ;; recorded as unexected
         (when on?
           (hash-set! actions-ht key #t))])))
   can-annotate)

  ;; filtered : (listof (list boolean srcloc))
  ;; remove redundant expressions
  (define filtered (hash-map actions-ht (λ (k v) (list v k))))

  (for/hash ([v filtered])
    (values (srcloc-source (cadr v))
            v)))
