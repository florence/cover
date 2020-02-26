#lang racket

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
         racket/set
         custom-load
         cover/private/shared
         cover/private/file-utils
         cover/private/format-utils
         cover/strace
         cover/cover)


(require rackunit racket/runtime-path compiler/cm compiler/compiler)

(require/expose
 cover/cover
 (with-cover-loggersf
  make-cover-load/use-compiled
  get-compile
  get-namespace))

(define-runtime-module-path prog cover/tests/prog)
(test-case "test for actually saving compiled files"
  (define prog.rkt (resolved-module-path-name prog))
  (define compiled
    (map simple-form-path
         (list
          (build-path prog.rkt ".." "compiled/prog_rkt.zo")
          (build-path prog.rkt ".." "compiled/prog_rkt.dep"))))
  (define (df)
    (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
              compiled))
  (test-begin
   (after
    (parameterize ([current-cover-environment (make-cover-environment)])
      (with-cover-loggersf
       (lambda ()
         (df)
         (check-false (ormap file-exists? compiled))
         (check-not-exn
          (lambda ()
            (define l/c (make-cover-load/use-compiled (list (->absolute prog.rkt))))
            (parameterize ([current-load/use-compiled l/c]
                           [current-compile (get-compile)]
                           [current-namespace (get-namespace)])
              (managed-compile-zo prog.rkt))))
         (check-true (andmap file-exists? compiled)))))
    (df))))

(require rackunit racket/runtime-path racket/file
         racket/format)
(define-runtime-module-path simple-multi/2 cover/tests/simple-multi/2)
(test-case "test repl like interactions"
  (define simple-multi/2.rkt (resolved-module-path-name simple-multi/2))
  (define env (make-cover-environment))
  (define ns (environment-namespace env))
  (parameterize ([current-cover-environment env]
                 [current-namespace ns])
    (with-cover-loggersf
     (lambda ()
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
                       (~a i))))))))


(test-case "test modules with same source and nondeterministic expansion"
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
        (namespace-require ''rand2)))))