#lang racket/base
(provide test-files!
         make-cover-environment
         get-test-coverage
         current-cover-environment environment?
         environment-compile environment-namespace
         coverage-wrapper-map)

#|

This module, and its partner strace.rkt implement code coverage.  In essence code coverage consists
of a protocol for between this file and all covered files, with the loger begin the communication
channel.

This module maintains a mapping between src locations in files to be covered and positions in a set
of vectors. The annotation in strace.rkt its given (mutable) access to this mapping, and an empty
set of vectors. During compilation the annotator will fill in the vector mapping and allocate the
vectors.

At annotate-module load time the module will log a request for its vector across using a pre-known
log topic combined with a key unique to each coverage environment. This module will log a responce
on a different pre-known key (combined with the same unique key). The data in this responce will be
the file->vector mapping.


Thus, In essence this module has three responsibilites:
1. setup a listening thread to responde to the protocol described above.
2. compile and run all test modules with the annotator and a coverage environment
3. interperet the coverage vectors into something useable.

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
         racket/syntax
         rackunit/log
         racket/list
         racket/port
         custom-load
         "private/shared.rkt"
         "private/file-utils.rkt"
         "private/format-utils.rkt"
         "strace.rkt")

;; An environment has:
;; a `namespace`
;; a handler for `current-compile`
;; a function that will annoate expanded code, given a file name
;; a topic for logs to be reiceved on. Must be unique for every environment
;; a hash map from srcloc to (List filename index)
;; a hash map from filename to [Vector bool]. Each vector location maps to a srcloc
;;   via the table in coverage-srcloc-mapping
(struct environment (namespace compile ann-top topic
                               coverage-srcloc-mapping
                               coverage-vector-mapping))
;; A special structure used for communicating information about programs that call `exit`
;; `code` is the exit code that `exit` was called with
(struct an-exit (code))

;;; ---------------------- Running Files ---------------------------------

;; Test files and build coverage map
;; returns true if no tests reported as failed, and no files errored.
(define (test-files! #:submod [submod-names 'test] #:env [env (current-cover-environment)]
                     #:dont-compile [dont-compile null]
                     . files)
  (parameterize ([current-cover-environment env])
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
    (define excludes (map ->absolute dont-compile))
    (define cover-load/use-compiled (make-cover-load/use-compiled abs-names))
    (define tests-failed
      (parameterize* ([current-load/use-compiled cover-load/use-compiled]
                      [current-namespace (get-namespace)])
        (with-cover-loggers
          (for ([f (in-list abs-names)]
                #:unless (member f excludes))
            (printf "cover: instrumenting: ~a\n" f)
            (compile-file f))
          (for*/fold ([tests-failed #f])
                     ([f (in-list abs)]
                      [submod-name (in-list (if (list? submod-names)
                                                submod-names
                                                (list submod-names)))])
            (printf "cover: running file: ~a\n" f)
            (define failed? (handle-file f submod-name))
            (or failed? tests-failed)))))
    (vprintf "ran ~s\n" files)
    (not tests-failed)))

(define-syntax-rule (with-cover-loggers e ...)
  (with-intercepted-logging/receiver (cover-give-file-mapping)
    (lambda () e ...)
    (make-log-receiver
     (current-logger)
     'info
     (format-symbol "~a~a" (get-topic) 'cover-internal-request-vector-mapping))))

;; we dont care what the msg content is, just send the vector back
(define (cover-give-file-mapping)
  (define topic (format-symbol "~a~a" (get-topic) 'cover-internal-send-vector-mapping))
  (lambda (_)
    (log-message (current-logger)
                 'info
                 topic
                 ""
                 (get-coverage-vector-mapping))))

;;; ---------------------- Running Aux ---------------------------------


;; PathString -> Boolean
(define (compile-file the-file)
  (parameterize ([current-compile (get-compile)]
                 [use-compiled-file-paths
                  (cons (build-path "compiled" "cover")
                        (use-compiled-file-paths))])
    (dynamic-require (build-file-require the-file) (void))))

;; (or PathString (list PathString Vector)) Symbol -> Boolean
;; returns true if any tests failed or errors occured
(define (handle-file maybe-path submod-name)
  (vprintf "attempting to run ~s in environment ~s\n" maybe-path (get-topic))
  (define the-file (if (list? maybe-path) (first maybe-path) maybe-path))
  (define argv (if (list? maybe-path) (second maybe-path) #()))

  (or (run-file! the-file submod-name argv)
      (tests-failed?)))

;; PathString Submod [Vectorof String] -> Boolean
(define (run-file! the-file submod-name argv)
  (define tests-errored #f)

  (with-handlers ([(lambda (x) (not (exn:break? x)))
                   (lambda (x)
                     (cond [(an-exit? x)
                            (vprintf "file ~s exited code ~s" the-file (an-exit-code x))]
                           [else
                            (set! tests-errored #t)
                            ((error-display-handler)
                             (if (exn? x)
                                 (exn-message x)
                                 "non-exn error:")
                             x)]))])
    (parameterize ([current-command-line-arguments argv]
                   [exit-handler (lambda (x) (raise (an-exit x)))])
      (vprintf "running file: ~s with args: ~s" the-file argv)
      (exec-file the-file submod-name)))

  tests-errored)

;; -> Bool
(define (tests-failed?)
  (define test-log (get-test-log))
  (let ([lg (test-log)])
    (and (not (= 0 (car lg)))
         (not (= 0 (cdr lg))))))

(define (get-test-log)
  (with-handlers ([exn:fail? (lambda _ (lambda () (cons 0 0)))])
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
  (vprintf "running ~s in environment ~s" to-run (get-topic))
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
      (define file (get-source e))
      (with-handlers ([void (lambda (e) (displayln file) (raise e))])
          (define to-compile
            (cond [(or (compiled-expression? (if (syntax? e) (syntax-e e) e))
                       (not (eq? reg (namespace-module-registry (current-namespace))))
                       (not file))
                   e]
                  [else
                   (vprintf "compiling ~s with coverage annotations in environment ~s"
                            file
                            (get-topic))
                   ((annotate-top file)
                             (if (syntax? e) (expand-syntax e) (datum->syntax #f e))
                             (namespace-base-phase (current-namespace)))]))
        (compile to-compile immediate-eval?))))
  cover-compile)

(define (get-source stx)
  (and (syntax? stx)
       (let loop ([e stx])
         (define f (syntax-source e))
         (define (do-loop)
           (define next (syntax->list e))
           (and next
                (ormap loop next)))
         (if f
             (if (path? f)
                 (path->string f)
                 f)
             (do-loop)))))

;;; ---------------------- Environments ---------------------------------

(define (make-cover-environment [ns (make-empty-namespace)])
  (kernelize-namespace! ns)
  (parameterize ([current-namespace ns])
    ;; we gensym the topic to isolate diverent coverage
    ;; instances from each other
    (define topic (gensym))
    (define loc-table (make-hash))
    (define vector-table (make-hash))
    (define ann (make-annotate-top topic loc-table vector-table))
    (environment
     ns
     (make-cover-compile ns ann)
     ann
     topic
     loc-table
     vector-table)))

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

(define (get-topic)
  (get-val environment-topic))

(define (get-coverage-srcloc-mapping)
  (get-val environment-coverage-srcloc-mapping))

(define (get-coverage-vector-mapping)
  (get-val environment-coverage-vector-mapping))

(struct coverage-wrapper (map function)
        #:property prop:procedure (struct-field-index function))

(require racket/pretty)
;; -> coverage/c
(define (get-test-coverage [env (current-cover-environment)])
  (parameterize ([current-cover-environment env])
    (vprintf "generating test coverage\n")

    (define vecmap (get-coverage-vector-mapping))
    (define raw-coverage
      (for*/hash ([(_ filemap) (in-hash (get-coverage-srcloc-mapping))]
                  [(srcloc spot) (in-hash filemap)])
        (match-define (list file loc) spot)
        (values srcloc
                (vector-ref
                 (hash-ref vecmap file)
                 loc))))


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
  (define (df)
    (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
              compiled))
  (test-begin
   (after
    (parameterize ([current-cover-environment (make-cover-environment)])
      (with-cover-loggers (df)
        (check-false (ormap file-exists? compiled))
        (check-not-exn
         (lambda ()
           (define l/c (make-cover-load/use-compiled (list (->absolute prog.rkt))))
           (parameterize ([current-load/use-compiled l/c]
                          [current-compile (get-compile)]
                          [current-namespace (get-namespace)])
             (managed-compile-zo prog.rkt))))
        (check-true (andmap file-exists? compiled))))
    (df))))

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
    (with-cover-loggers
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
                      (~a i)))))))

;; test modules with same source and nondeterministic expansion
(module+ test
  (test-begin
    (for ([i (in-range 10)])
      (define env (make-cover-environment))
      (define ns (environment-namespace env))
      (define path "/a/b/c")
      (define l/c (make-cover-load/use-compiled (list path)))
      (parameterize ([current-cover-environment env]
                     [current-namespace ns])
        (namespace-require 'racket/base)
        (define mod-rand
          `(module rand racket
              (require (for-syntax racket))
              (define-syntax (f stx)
                (datum->syntax stx ''0
                               (vector
                                ,path
                                #f
                                #f
                                1
                                (random 2))))
              (void (f))))
        (define mod-load
          '(module rand2 racket
              (require (for-syntax 'rand) 'rand)))
        (parameterize ([current-load/use-compiled l/c]
                       [current-compile (get-compile)])
          (eval mod-rand)
          (eval mod-load)
          (namespace-require ''rand2))))))
