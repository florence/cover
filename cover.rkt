#lang racket/base
(provide test-files! clear-coverage! get-test-coverage)
(require (for-syntax racket/base))
(require racket/dict
         syntax/modcode
         racket/function
         syntax/modread
         syntax/parse
         unstable/syntax
         racket/runtime-path
         rackunit
         unstable/error
         racket/list
         racket/port
         "private/shared.rkt"
         "private/file-utils.rkt")

(define ns #f)

;; PathString * -> Boolean
;; Test files and build coverage map
;; returns true if all tests passed
(define (test-files!  #:submod [submod-name 'test] . paths)
  (unless ns (unloaded-error))
  (define abs
    (for/list ([p (in-list paths)])
      (if (list? p)
          (cons (->absolute (car p)) (cdr p))
          (->absolute p))))
  (parameterize ([current-load/use-compiled (make-cover-load/use-compiled paths)]
                 [use-compiled-file-paths
                  (cons (build-path "compiled" "cover")
                        (use-compiled-file-paths))]
                 [current-compile (make-cover-compile)]
                 [current-output-port
                  (if (verbose) (current-output-port) (open-output-nowhere))])
    (define tests-failed #f)
    (for ([p (in-list paths)])
      (vprintf "attempting to run ~s\n" p)
      (define old-check (current-check-handler))
      (define path (if (list? p) (car p) p))
      (define argv (if (list? p) (cadr p) #()))
      (vprintf "running file: ~s with args: ~s\n" path argv)
      (struct an-exit (code))
      (with-handlers ([(lambda (x) (or (not (exn? x)) (exn:fail? x)))
                       (lambda (x)
                         (cond [(an-exit? x)
                                (vprintf "file ~s exited code ~s" p (an-exit-code x))]
                               [else
                                (set! tests-failed #t)
                                (error-display x)]))])
        (parameterize* ([current-command-line-arguments argv]
                        [exit-handler (lambda (x) (raise (an-exit x)))]
                        [current-namespace ns]
                        [(get-check-handler-parameter)
                         (lambda x
                           (set! tests-failed #t)
                           (vprintf "file ~s had failed tests\n" p)
                           (apply old-check x))])
          (define file `(file ,(if (path? path) (path->string path) path)))
          (define submod `(submod ,file ,submod-name))
          (run-mod (if (module-declared? submod #t) submod file)))))
    (vprintf "ran ~s\n" paths)
    (not tests-failed)))

(define (run-mod to-run)
  (vprintf "running ~s\n" to-run)
  (eval `(dynamic-require ',to-run #f))
  (vprintf "finished running ~s\n" to-run))

(define o (current-output-port))
(define (make-cover-load/use-compiled paths)
  (define load/use-compiled (current-load/use-compiled))
  (define load (current-load))
  (lambda (path sym)
    (define abs (->absolute path))
    (define lst (explode-path abs))
    (define dir-list (take lst (sub1 (length lst))))
    (parameterize ([current-load-relative-directory (apply build-path dir-list)])
      (if (member abs paths)
          (load path sym)
          (load/use-compiled path sym)))))

(define (make-cover-compile)
  (define compile (current-compile))
  (define reg (namespace-module-registry ns))
  (define phase (namespace-base-phase ns))
  (define annotate-top (get-annotate-top))
  (lambda (e immediate-eval?)
    (define to-compile
      (cond [(or (compiled-expression? (if (syntax? e) (syntax-e e) e))
                 (not (eq? reg (namespace-module-registry (current-namespace))))
                 (not (equal? phase (namespace-base-phase (current-namespace)))))
             e]
            [else
             (vprintf "compiling ~s with coverage annotations\n"
                      (if (not (syntax? e))
                          e
                          (or (syntax-source-file-name e)
                              (syntax-source e)
                              (syntax->datum e))))
             (annotate-top (namespace-syntax-introduce
                            (if (syntax? e) (expand-syntax e) (datum->syntax #f e)))
                           phase)]))
    (compile to-compile immediate-eval?)))

(define-runtime-path cov "coverage.rkt")
(define-runtime-path strace "strace.rkt")
;; -> Void
;; clear coverage map
(define (clear-coverage!)
  (set! ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require `(file ,(path->string cov)))
    (namespace-require `(file ,(path->string strace)))
    (namespace-require 'rackunit))
  (load-annotate-top!)
  (load-raw-coverage!)
  (load-current-check-handler!))

;; -> [Hashof PathString (Listof (List Boolean srcloc))]
;; returns a hash of file to a list, where the first of the list is if
;; that srcloc was covered or not
;; based on <pkgs>/drracket/drracket/private/debug.rkt
(define (get-test-coverage)
  (vprintf "generating test coverage\n")
  ;; can-annotate : (listof (list boolean srcloc))
  ;; boolean is #t => code was run
  ;;            #f => code was not run
  ;; remove those that cannot be annotated
  (define can-annotate
    (filter values
            (for/list ([(stx covered?) (in-hash (get-raw-coverage))])
              (and (syntax? stx)
                   (let* ([orig-src (syntax-source stx)]
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

  (for ([v (in-list filtered)])
    (define file (srcloc-source (cadr v)))
    (hash-update! out
                  file
                  (lambda (l) (cons v l))
                  null))
  out)

(define ann-top #f)
(define (get-annotate-top)
  (or ann-top (unloaded-error)))
(define (load-annotate-top!)
  (set! ann-top (get-ns-var 'annotate-top)))

(define raw-cover #f)
(define (get-raw-coverage)
  (or raw-cover (unloaded-error)))
(define (load-raw-coverage!)
  (set! raw-cover (get-ns-var 'coverage)))

(define cch #f)
(define (load-current-check-handler!)
  (set! cch (get-ns-var 'current-check-handler)))
(define (get-check-handler-parameter)
  (or cch (unloaded-error)))

(define (unloaded-error)
  (error 'cover "Test coverage not loaded."))

(define (get-ns-var sym)
  (namespace-variable-value sym #t #f ns))

;; A little hack to setup coverage for the first time
(clear-coverage!)
