#lang racket
(require cover rackunit racket/runtime-path version/utils)
(define-runtime-path-list fs
  (list "module.rkt"
        "bfs.rkt"))
(define-runtime-path-list others
  (list "bfs+module-nolex.rkt"
        "bfs+module.rkt"
        "bfs+define-syntax.rkt"
        "lazy-require.rkt"))
(test-case
 "begin-for-syntax with modules should be okay"
 (parameterize ([current-cover-environment (make-cover-environment)])
   (for-each
    (lambda (f)
      (check-not-exn
       (lambda () (test-files! f))
       (path->string f)))
    (append fs
            ;; we do not support version module*'s in begin-for-syntax
            ;; on the old expander
            (if (version<=? (version) "6.2.900")
                null
                others)))))
