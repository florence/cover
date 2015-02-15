#lang racket/base
(provide test-files! clear-coverage! get-test-coverage)

#|

This module implements code coverage. It works by compiling and running the given modules with in a
separate namespace errortrace annotations that write coverage information to a hashmap exported from
in "coverage.rkt". This raw coverage information is converted to a usable form by
`get-test-coverage`.

|#

(require (for-syntax racket/base))
(require racket/dict
         syntax/modcode
         racket/function
         syntax/modread
         syntax/parse
         unstable/syntax
         racket/runtime-path
         racket/match
         rackunit
         unstable/error
         racket/list
         racket/port
         "private/shared.rkt"
         "private/file-utils.rkt")

;; namespace used for coverage
(define ns #f)

;; Test files and build coverage map
;; returns true if no tests reported as failed, and no files errored.
(define (test-files!  #:submod [submod-name 'test] . files)
  (unless ns (unloaded-error))
  (define abs
    (for/list ([p (in-list files)])
      (if (list? p)
          (cons (->absolute/port (car p)) (cdr p))
          (->absolute/port p))))
  (define abs-names
    (for/list ([p abs])
      (match p
        [(cons p _) p]
        [(? input-port? p)
         (object-name p)]
        [_ p])))
  (parameterize ([current-load/use-compiled (make-cover-load/use-compiled abs-names)]
                 [current-output-port
                  (if (verbose) (current-output-port) (open-output-nowhere))])
    (define tests-failed #f)
    (for ([p (in-list abs)])
      (vprintf "attempting to run ~s\n" p)
      (define old-check (current-check-handler))
      (define the-file (if (list? p) (car p) p))
      (define argv (if (list? p) (cadr p) #()))
      (vprintf "running file: ~s with args: ~s\n" the-file argv)
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
          (run-file the-file submod-name))))
    (vprintf "ran ~s\n" files)
    (remove-unneeded-results! abs-names)
    (not tests-failed)))

;; (U InputPort PathString) -> (U InputPort PathString)
;; like ->absolute but handles ports
(define (->absolute/port p)
  (if (port? p) p (->absolute p)))

(define (run-file the-file submod-name)
  (cond [(input-port? the-file)
         (eval (read-syntax (object-name the-file) the-file))]
        [else
         (define sfile `(file ,(if (path? the-file) (path->string the-file) the-file)))
         (define submod `(submod ,sfile ,submod-name))
         (run-mod (if (module-declared? submod #t) submod sfile))]))

;; ModulePath -> Void
;; evaluate the current module in the current namespace
(define (run-mod to-run)
  (vprintf "running ~s\n" to-run)
  (eval `(dynamic-require ',to-run 0))
  (vprintf "finished running ~s\n" to-run))

;; [Listof Path] -> Loader Compiler
;; returns a value that can be set of `current-load/use-compiled`
;; forces the given files to be recompiled whenever load/use-compiled is called
(define (make-cover-load/use-compiled paths)
  (define load/use-compiled (current-load/use-compiled))
  (define load (current-load))
  (define cover-compile (make-cover-compile))
  (define cover-use-compiled-file-paths
    (cons (build-path "compiled" "cover")
          (use-compiled-file-paths)))
  (lambda (path sym)
    (define abs (->absolute path))
    (define lst (explode-path abs))
    (define dir-list (take lst (sub1 (length lst))))
    (parameterize ([current-load-relative-directory (apply build-path dir-list)])
      (if (member abs paths)
          (parameterize ([current-compile cover-compile]
                         [use-compiled-file-paths
                          cover-use-compiled-file-paths])
            (load path sym))
          (load/use-compiled path sym)))))

;; -> Compiler
;; makes a value sutable for current-compile, such that compile
;; annotates the source code. should only be used by `make-cover-load/uze-compiled`
(define (make-cover-compile)
  (define compile (current-compile))
  (define reg (namespace-module-registry ns))
  (define phase (namespace-base-phase ns))
  (define annotate-top (get-annotate-top))
  ;; define so its named in stack traces
  (define cover-compile
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
               (annotate-top (if (syntax? e) (expand-syntax e) (datum->syntax #f e))
                             phase)]))
      (compile to-compile immediate-eval?)))
  cover-compile)

;; [Listof Any] -> Void
;; remove any files not in paths from the raw coverage
(define (remove-unneeded-results! names)
  (define c (get-raw-coverage))
  (for ([s (in-list (hash-keys c))]
        #:when (not (member (srcloc-source s) names)))
    (hash-remove! c s)))

;; -> Void
;; clear coverage map. Effectively recreates and rebuilds `ns`
(define (clear-coverage!)
  (set! ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'cover/coverage)
    (namespace-require 'cover/strace)
    (namespace-require 'rackunit))
  (load-names!))

;; -> Void
;; loads any needed names from `ns` before it can get polluted.
(define (load-names!)
  (load-annotate-top!)
  (load-raw-coverage!)
  (load-current-check-handler!))

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


;; -> [Hashof PathString (Listof (List Boolean srcloc))]
;; returns a hash of file to a list, where the first of the list is if
;; that srcloc was covered or not
;; based on <pkgs>/drracket/drracket/private/debug.rkt
(define (get-test-coverage)
  (vprintf "generating test coverage\n")

  ;; filtered : (listof (list boolean srcloc))
  ;; remove redundant expressions
  (define filtered (hash-map (get-raw-coverage) (λ (k v) (list v k))))

  (define out (make-hash))

  (for ([v (in-list filtered)])
    (define file (srcloc-source (cadr v)))
    (hash-update! out
                  file
                  (lambda (l) (cons v l))
                  null))
  out)

;; A little hack to setup coverage namespace for the first time
(clear-coverage!)


;; here live tests for actually saving compiled files
(module+ test
  (require rackunit racket/runtime-path compiler/cm compiler/compiler)
  (define-runtime-path prog.rkt "tests/prog.rkt")
  (define-runtime-path-list compiled
    (list
     "tests/compiled/prog_rkt.zo"
     "tests/compiled/prog_rkt.dep"))
  (test-begin
   (after
    (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
              compiled)
    (check-false (ormap file-exists? compiled))
    (check-not-exn
     (lambda ()
       (parameterize ([current-load/use-compiled
                       (make-cover-load/use-compiled (list (->absolute prog.rkt)))]
                      [current-namespace ns])
         (managed-compile-zo prog.rkt))))
    (check-true (andmap file-exists? compiled))
    (clear-coverage!))))
