#lang racket/base
(provide test-files!
         make-cover-environment clear-coverage!
         get-test-coverage
         current-cover-environment environment?
         environment-compile environment-namespace
         coverage-wrapper-map)

#|

This module implements code coverage. It works by compiling and running the given modules with in a
separate namespace errortrace annotations that log coverage information. This raw coverage
information is converted to a usable form by `get-test-coverage`.

|#

(require (for-syntax racket/base))
(require racket/dict
         syntax/modcode
         racket/function
         syntax/modread
         syntax/modresolve
         syntax/parse
         racket/bool
         racket/runtime-path
         racket/match
         racket/path
         rackunit/log
         unstable/error
         racket/list
         racket/port
         custom-load
         "private/shared.rkt"
         "private/file-utils.rkt"
         "private/format-utils.rkt"
         "strace.rkt")

;; An environment has:
;; a `namespace`, which shall always have `coverage.rkt` and ''#%builtin attached
;; a handler for `current-compile`
;; a function that will annoate expanded code
;; a topic for logs to be reiceved on. Must be unique for every environment
;; a log receiver, for receiving log events about coverage
;; a hash map to store raw coverage read from the receiver
(struct environment (namespace compile ann-top receiver topic raw-coverage))
;; A special structure used for communicating information about programs that call `exit`
;; `code` is the exit code that `exit` was called with
(struct an-exit (code))

;;; ---------------------- Running Files ---------------------------------

;; Test files and build coverage map
;; returns true if no tests reported as failed, and no files errored.
(define (test-files! #:submod [submod-name 'test] #:env [env (current-cover-environment)] . files)
  (parameterize ([current-cover-environment env])
    (with-intercepted-logging/receiver (cover-receiver (get-raw-coverage-map))
      (lambda ()
        (define abs
          (for/list ([p (in-list files)])
            (if (list? p)
                (cons (->absolute (car p)) (cdr p))
                (->absolute p))))
        (define abs-names
          (for/list ([p (in-list abs)])
            (match p
              [(cons p _) p]
              [_ p])))
        (define cover-load/use-compiled (make-cover-load/use-compiled abs-names))
        (define tests-failed
          (parameterize* ([current-load/use-compiled cover-load/use-compiled]
                          [current-namespace (get-namespace)])
            (for ([f (in-list abs-names)])
              (vprintf "forcing compilation of ~a" f)
              (compile-file f))
            (for/fold ([tests-failed #f]) ([f (in-list abs)])
              (define failed? (handle-file f submod-name))
              (or failed? tests-failed))))
        (vprintf "ran ~s\n" files)
        (not tests-failed))
      (get-receiver))))

;;; ---------------------- Running Aux ---------------------------------


;; PathString -> Void
(define (compile-file the-file)
  (parameterize ([current-compile (get-compile)]
                 [use-compiled-file-paths
                  (cons (build-path "compiled" "cover")
                        (use-compiled-file-paths))])
    (dynamic-require (build-file-require the-file) (void))))

;; (or PathString (list PathString Vector)) Symbol -> Boolean
;; returns true if any tests failed or errors occured
(define (handle-file maybe-path submod-name)
  (define tests-errored #f)
  (vprintf "attempting to run ~s in environment ~s\n" maybe-path (get-topic))
  (define the-file (if (list? maybe-path) (first maybe-path) maybe-path))
  (define argv (if (list? maybe-path) (second maybe-path) #()))
  (with-handlers ([(lambda (x) (not (exn:break? x)))
                   (lambda (x)
                     (cond [(an-exit? x)
                            (vprintf "file ~s exited code ~s" maybe-path (an-exit-code x))]
                           [else
                            (set! tests-errored #t)
                            (error-display x)]))])
    (parameterize ([current-command-line-arguments argv]
                   [exit-handler (lambda (x) (raise (an-exit x)))])
      (vprintf "running file: ~s with args: ~s" the-file argv)
      (exec-file the-file submod-name)))
  (define test-log (get-test-log))
  (or tests-errored
      (let ([lg (test-log)])
        (and (not (= 0 (car lg)))
             (not (= 0 (cdr lg)))))))

(define (get-test-log)
  (with-handlers ([exn:fail? (lambda _
                               (lambda () (cons 0 0)))])
    (parameterize ([current-namespace (get-namespace)])
      (module->namespace 'rackunit/log);make sure its loaded first
      (dynamic-require 'rackunit/log 'test-log))))

;; PathString Symbol -> Void
(define (exec-file the-file submod-name)
  (define sfile (build-file-require the-file))
  (define submod `(submod ,sfile ,submod-name))
  (run-mod (if (module-declared? submod #t) submod sfile)))

;; ModulePath -> Any
(define (run-mod to-run)
  (vprintf "running ~s in envoronment ~s" to-run (get-topic))
  (dynamic-require to-run 0)
  (vprintf "finished running ~s" to-run))

;; PathString -> ModulePath
(define (build-file-require the-file)
  `(file ,(if (path? the-file) (path->string the-file) the-file)))

;;; ---------------------- Compiling ---------------------------------

;; (U [Listof Path] #f) -> load/use-compiled
;; returns a value that can be set to `current-load/use-compiled`
;; forces the given files to be recompiled whenever load/use-compiled is called
(define (make-cover-load/use-compiled paths)
  (make-custom-load/use-compiled
   #:blacklist (for/list ([p (in-list paths)])
                 (regexp (regexp-quote p)))))

;; -> Compiler
;; makes a value sutable for current-compile, such that compile
;; annotates the source code with annotate-top. meant to be called
;; only by make-cover-environment
(define (make-cover-compile ns annotate-top)
  (define compile (current-compile))
  (define reg (namespace-module-registry ns))
  (define phase (namespace-base-phase ns))
  ;; define so its named in stack traces
  (define cover-compile
    (lambda (e immediate-eval?)
      (define to-compile
        (cond [(or (compiled-expression? (if (syntax? e) (syntax-e e) e))
                   (not (eq? reg (namespace-module-registry (current-namespace)))))
              e]
              [else
               (define fname
                 (if (not (syntax? e))
                     e
                     (or (syntax-source e)
                         (syntax->datum e))))
               (vprintf "compiling ~s with coverage annotations in enviornment ~s"
                        fname
                        (get-topic))
               (let ([x (annotate-top (if (syntax? e) (expand-syntax e) (datum->syntax #f e))
                                      (namespace-base-phase (current-namespace)))])
                 (vprintf "current map size is: ~a after compiling ~s\n"
                          (hash-count (get-raw-coverage-map))
                          fname)
                 x)]))
      (compile to-compile immediate-eval?)))
  cover-compile)

;;; ---------------------- Environments ---------------------------------

(define (clear-coverage!)
  (current-cover-environment (make-cover-environment)))

(define (make-cover-environment [ns (make-empty-namespace)])
  (kernelize-namespace! ns)
  (parameterize ([current-namespace ns])
    ;; we gensym the topic to isolate diverent coverage
    ;; instances from each other
    (define topic (gensym))
    (define ann (make-annotate-top topic))
    (environment
     ns
     (make-cover-compile ns ann)
     ann
     (make-receiver topic)
     topic
     (make-hash))))

(define (make-receiver topic)
  (make-log-receiver (current-logger) 'info topic))

(define (kernelize-namespace! ns)
  (define cns (current-namespace))
  (namespace-attach-module cns ''#%builtin ns))

(define (get-annotate-top)
  (get-val environment-ann-top))
(define (load-annotate-top)
  (make-annotate-top))

(define (get-namespace)
  (get-val environment-namespace))

(define (get-compile)
  (get-val environment-compile))

(define (get-val access)
  (access (current-cover-environment)))

(define (get-receiver)
  (get-val environment-receiver))

(define (get-raw-coverage-map)
  (get-val environment-raw-coverage))

(define (get-topic)
  (get-val environment-topic))

(struct coverage-wrapper (map function)
        #:property prop:procedure (struct-field-index function))

;; -> coverage/c
(define (get-test-coverage [env (current-cover-environment)])
  (parameterize ([current-cover-environment env])
    (vprintf "generating test coverage\n")
    (define raw-coverage (get-raw-coverage-map))
    (define r (get-receiver))
    (define receive (cover-receiver raw-coverage))

    (let loop ()
      (define v (sync/timeout (lambda () #f) r))
      (when v
        (receive v)
        (loop)))

    ;; filtered : (listof (list boolean srcloc))
    (define filtered (hash-map raw-coverage
                               (λ (k v) (list v (apply make-srcloc k)))))

    (define out (make-hash))

    (for ([v (in-list filtered)])
      (define file (srcloc-source (cadr v)))
      (hash-update! out
                    file
                    (lambda (l) (cons v l))
                    null))

    ;; Make the hash map immutable
    (define coverage (for/hash ([(k v) (in-hash out)]) (values k v)))
    (define file-map (make-hash))
    (coverage-wrapper
     coverage
     (lambda (key location)
       (define f
         (hash-ref! file-map key
                    (lambda ()
                      (make-covered? coverage key))))
       (f location)))))

(define ((cover-receiver raw-coverage) msg)
  (match msg
    [(vector info type data _)
     (cond [(regexp-match? (regexp-quote logger-init-message) type)
            (unless (hash-has-key? raw-coverage data)
              (hash-set! raw-coverage data #f))]
           [(regexp-match? (regexp-quote logger-covered-message) type)
            (hash-set! raw-coverage data #t)])]))

(define current-cover-environment
  (make-parameter (make-cover-environment)))

;; here live tests for actually saving compiled files
(module+ test
  (require rackunit racket/runtime-path compiler/cm compiler/compiler)
  (define-runtime-path prog.rkt "tests/prog.rkt")
  (define-runtime-path-list compiled
    (list
     "tests/compiled/prog_rkt.zo"
     "tests/compiled/prog_rkt.dep"))
  (test-begin
   (parameterize ([current-cover-environment (make-cover-environment)])
     (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
               compiled)
     (check-false (ormap file-exists? compiled))
     (check-not-exn
      (lambda ()
        (define l/c (make-cover-load/use-compiled (list (->absolute prog.rkt))))
        (parameterize ([current-load/use-compiled l/c]
                       [current-compile (get-compile)]
                       [current-namespace (get-namespace)])
          (managed-compile-zo prog.rkt))))
     (check-true (andmap file-exists? compiled)))))

;; tests repl like interactions
(module+ test
  (require rackunit racket/runtime-path racket/file
           racket/format
           racket/lazy-require)
  ;; break cyclic dependency in testing
  (define-runtime-path simple-multi/2.rkt "tests/simple-multi/2.rkt")
  (define env (make-cover-environment))
  (define ns (environment-namespace env))
  (parameterize ([current-cover-environment env]
                 [current-namespace ns])
    (namespace-require 'racket/base)
    (test-begin
     (define file (path->string simple-multi/2.rkt))
     (define modpath file)
     (define l/c (make-cover-load/use-compiled (list file)))
     (parameterize ([current-load/use-compiled l/c]
                    [current-compile (get-compile)])
       (namespace-require `(file ,modpath)))
     (check-equal? (eval `(two)) 10)
     (define x (get-test-coverage env))
     (define covered? (curry x file))
     (for ([_ (in-string (file->string file))]
           [i (in-naturals 1)])
       (check-not-exn (thunk (covered? i)))
       (define c (covered? i))
       (check-true  (or (eq? c 'covered)
                        (eq? c 'irrelevant))
                    (~a i))))))
