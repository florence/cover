#lang racket/base
(provide test-files! cover-module!
         make-clean-cover-environment clear-coverage!
         get-test-coverage
         current-cover-environment environment?
         environment-compile environment-namespace)

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
         syntax/modresolve
         syntax/parse
         unstable/syntax
         racket/bool
         racket/runtime-path
         racket/match
         rackunit
         unstable/error
         racket/list
         racket/port
         "private/shared.rkt"
         "private/file-utils.rkt")

(struct environment (namespace compile ann-top raw-cover cch))

;;; ---------------------- Running Files ---------------------------------

;; Test files and build coverage map
;; returns true if no tests reported as failed, and no files errored.
(define (test-files! #:submod [submod-name 'test] #:env [env (current-cover-environment)] . files)
  (parameterize ([current-cover-environment env])
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
        (parameterize* ([current-load/use-compiled (make-cover-load/use-compiled abs-names)]
                        [current-output-port
                         (if (verbose) (current-output-port) (open-output-nowhere))]
                        [current-command-line-arguments argv]
                        [exit-handler (lambda (x) (raise (an-exit x)))]
                        [current-namespace (get-namespace)]
                        [(get-check-handler-parameter)
                         (lambda x
                           (set! tests-failed #t)
                           (vprintf "file ~s had failed tests\n" p)
                           (apply old-check x))])
          (run-file the-file submod-name))))
    (vprintf "ran ~s\n" files)
    (remove-unneeded-results! abs-names)
    (not tests-failed)))

;; ResolvedModulePath -> Void
;; visit and instantiate the given module path in the cover environment
(define (cover-module! p [env (current-cover-environment)])
  (define modpath (->absolute p))
  (define to-run `(file ,modpath))
  (parameterize* ([current-cover-environment env]
                  [current-load/use-compiled (make-cover-load/use-compiled (list modpath))]
                  [current-compile (get-compile)]
                  [current-namespace (get-namespace)])
    (eval (make-dyn-req-expr to-run))))

;;; ---------------------- Running Aux ---------------------------------

(define (run-file the-file submod-name)
  (cond [(input-port? the-file)
         (eval (read-syntax (object-name the-file) the-file))]
        [else
         (define sfile `(file ,(if (path? the-file) (path->string the-file) the-file)))
         (define submod `(submod ,sfile ,submod-name))
         (run-mod (if (module-declared? submod #t) submod sfile))]))

(define (run-mod to-run)
  (vprintf "running ~s\n" to-run)
  (eval (make-dyn-req-expr to-run))
  (vprintf "finished running ~s\n" to-run))

;; (U InputPort PathString) -> (U InputPort PathString)
;; like ->absolute but handles ports
(define (->absolute/port p)
  (if (port? p) p (->absolute p)))

(define (make-dyn-req-expr to-run)
  `(dynamic-require ',to-run 0))

;; [Listof Any] -> Void
;; remove any files not in paths from the raw coverage
(define (remove-unneeded-results! names)
  (define c (get-raw-coverage))
  (for ([s (in-list (hash-keys c))]
        #:when (not (member (srcloc-source s) names)))
    (hash-remove! c s)))

;;; ---------------------- Compiling ---------------------------------

;; (U [Listof Path] #f) -> Loader Compiler
;; returns a value that can be set of `current-load/use-compiled`
;; forces the given files to be recompiled whenever load/use-compiled is called
(define (make-cover-load/use-compiled paths)
  (define load/use-compiled (current-load/use-compiled))
  (define load (current-load))
  (define cover-compile (get-compile))
  (define cover-use-compiled-file-paths
    (cons (build-path "compiled" "cover")
          (use-compiled-file-paths)))
  (lambda (path sym)
    (define abs (->absolute path))
    (define lst (explode-path abs))
    (define dir-list (take lst (sub1 (length lst))))
    (parameterize ([current-load-relative-directory (apply build-path dir-list)])
      (if (implies paths (member abs paths))
          (parameterize ([current-compile cover-compile]
                         [use-compiled-file-paths
                          cover-use-compiled-file-paths])
            (load path sym))
          (load/use-compiled path sym)))))

;; -> Compiler
;; makes a value sutable for current-compile, such that compile
;; annotates the source code with annotate-top. meant to be called
;; only by initialize-cover-environment
(define (make-cover-compile ns annotate-top)
  (define compile (current-compile))
  (define reg (namespace-module-registry ns))
  (define phase (namespace-base-phase ns))
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

;;; ---------------------- Environments ---------------------------------

(define (clear-coverage!)
  (current-cover-environment (make-clean-cover-environment)))

(define (make-clean-cover-environment)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'cover/coverage)
    (namespace-require 'cover/strace)
    (namespace-require 'rackunit)
    (define ann (load-annotate-top))
    (environment
     ns
     (make-cover-compile ns ann)
     ann
     (load-raw-coverage)
     (load-current-check-handler))))

;; -> Void
;; loads any needed names from `ns` before it can get polluted.
(define (load-names)
  (load-annotate-top)
  (load-raw-coverage)
  (load-current-check-handler))

(define (get-annotate-top)
  (get-val environment-ann-top))
(define (load-annotate-top)
  (get-namespace-var 'annotate-top))

(define (get-raw-coverage)
  (get-val environment-raw-cover))
(define (load-raw-coverage)
  (get-namespace-var 'coverage))

(define (get-check-handler-parameter)
  (get-val environment-cch))
(define (load-current-check-handler)
  (get-namespace-var 'current-check-handler))

(define (get-namespace)
  (get-val environment-namespace))

(define (get-compile)
  (get-val environment-compile))

(define (get-val access)
  (access (current-cover-environment)))

(define (get-namespace-var sym)
  (namespace-variable-value sym #t #f (current-namespace)))

;; -> [Hashof PathString (Listof (List Boolean srcloc))]
;; returns a hash of file to a list, where the first of the list is if
;; that srcloc was covered or not
;; based on <pkgs>/drracket/drracket/private/debug.rkt
(define (get-test-coverage [env (current-cover-environment)])
  (parameterize ([current-cover-environment env])
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
    out))

(define current-cover-environment
  (make-parameter (make-clean-cover-environment)))

;; here live tests for actually saving compiled files
(module+ test
  (require rackunit racket/runtime-path compiler/cm compiler/compiler)
  (define-runtime-path prog.rkt "tests/prog.rkt")
  (define-runtime-path-list compiled
    (list
     "tests/compiled/prog_rkt.zo"
     "tests/compiled/prog_rkt.dep"))
  (test-begin
   (parameterize ([current-cover-environment (make-clean-cover-environment)])
     (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
               compiled)
     (check-false (ormap file-exists? compiled))
     (check-not-exn
      (lambda ()
        (parameterize ([current-load/use-compiled
                        (make-cover-load/use-compiled (list (->absolute prog.rkt)))]
                       [current-namespace (get-namespace)])
          (managed-compile-zo prog.rkt))))
     (check-true (andmap file-exists? compiled)))))
