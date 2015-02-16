#lang racket
(module+ test
  (require rackunit "../main.rkt" racket/runtime-path)
  (define-runtime-path simple-multi/2.rkt "simple-multi/2.rkt")
  (define env (make-clean-cover-environment))
  (test-begin
   (define file (path->string simple-multi/2.rkt))
   (define modpath file)
   (cover-module! modpath env)
   (define ns (environment-namespace env))
   (eval `(require (file ,modpath)) ns)
   (check-equal? (eval `(two) ns) 10)
   (define x (get-test-coverage env))
   (define covered?
     (make-covered? (hash-ref x file) file))
   (for ([_ (in-string (file->string file))]
         [i (in-naturals 1)])
     (check-equal? (covered? i) 'covered (~a i)))))
