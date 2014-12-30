#lang racket/base
(provide test-files! clear-coverage! get-test-coverage)
(require (for-syntax racket/base))
(require racket/dict
         syntax/modcode
         racket/function
         syntax/modread
         syntax/parse
         racket/runtime-path
         rackunit)



(define ns (make-base-namespace))

;; PathString * -> Boolean
;; Test files and build coverage map
;; returns true if all tests passed
(define (test-files! . paths)
  (clear-coverage!)
  (for ([p (map simplify-path paths)])
    (let loop ()
      (define-values (loc type) (get-module-path (build-path p)))
      (case type
        [(zo so)
         (delete-file loc)
         (loop)]
        [else (void)])))
  (parameterize ([use-compiled-file-paths
                  (cons (build-path "compiled" "better-test")
                        (use-compiled-file-paths))]
                 [current-compile (make-better-test-compile)])
    (define tests-failed #f)
    (for ([p paths])
      (define old-check (current-check-handler))
      (parameterize* ([current-namespace ns]
                      [current-check-handler
                       (lambda x
                         (set! tests-failed #t)
                         (apply old-check x))])
        (eval `(dynamic-require '(file ,p) #f))
        (namespace-require `(file ,p))
        (define submod `(submod (file ,p) test))
        (when (module-declared? submod)
          (namespace-require submod))))
    (not tests-failed)))

(define (make-better-test-compile)
  (define compile (current-compile))
  (define reg (namespace-module-registry ns))
  (define phase (namespace-base-phase ns))
  (define annotate-top (get-annotate-top))
  (lambda (e immediate-eval?)
    (define to-compile
      (if (eq? reg (namespace-module-registry (current-namespace)))
          (annotate-top
           (if (syntax? e) (expand e) (datum->syntax #f e))
           phase)
          e))
    (compile to-compile immediate-eval?)))

(define-runtime-path cov "coverage.rkt")
(define-runtime-path strace "strace.rkt")
;; -> Void
;; clear coverage map
(define (clear-coverage!)
  ;(dict-clear! coverage)
  (set! ns (make-base-namespace))
  ;(namespace-attach-module (current-namespace) cov ns)
  (namespace-attach-module (current-namespace) 'rackunit ns)
  (parameterize ([current-namespace ns])
    (namespace-require `(file ,(path->string cov)))
    (namespace-require `(file ,(path->string strace)))
    (namespace-require 'rackunit)))

;; -> [Hashof PathString (Listof (List Boolean srcloc))]
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
            (for/list ([(stx covered?) (get-raw-coverage)])
              (and (syntax? stx)
                   (let* ([orig-src  (syntax-source stx)]
                          [src (if (path? orig-src) (path->string orig-src) orig-src)]
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
        [(eq? old 'nothing)
         (hash-set! actions-ht key on?)]
        [old ;; recorded as executed
         (void)]
        [(not old) ;; recorded as unexected
         (when on?
           (hash-set! actions-ht key #t))])))
   can-annotate)

  ;; filtered : (listof (list boolean srcloc))
  ;; remove redundant expressions
  (define filtered (hash-map actions-ht (λ (k v) (list v k))))

  (define out (make-hash))

  (for ([v filtered])
    (define file (srcloc-source (cadr v)))
    (hash-update! out
                  file
                  (lambda (l) (cons v l))
                  null))
  out)

(define (get-annotate-top)
  (get-ns-var 'annotate-top))
(define (get-raw-coverage)
  (get-ns-var 'coverage))
(define (get-ns-var sym)
  (namespace-variable-value sym #t #f ns))
