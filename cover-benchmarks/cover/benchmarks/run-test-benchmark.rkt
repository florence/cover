#lang racket/base
(require cover/raco custom-load
         cover/cover
         cover/private/shared
         racket/port)

(module+ main
  (require racket/cmdline)
  (define-values (type things)
    (command-line
     #:args (type . things)
     (values (string->symbol type) things)))
  (benchmark type things))
  


(define (benchmark type things)
  (define p (make-empty-namespace))
  (kernelize-namespace! p)
  (define f (get-the-files type things))
  (parameterize ([current-namespace p]
                 [current-output-port (open-output-nowhere)])
    (build-files f)
    (test-files f)))
    
(define (get-the-files type things)
  (define-values (test _)
    (get-files
     type
     things
     '()
     '()))
  test)

(define (build-files f)
  (parameterize
      ([current-load/use-compiled
        (make-custom-load/use-compiled
         #:blacklist (map (lambda (x) (regexp (regexp-quote (if (list? x) (car x) x)))) f))])
    (define-values (_ t1 t2 t3)
      (time-apply
       (lambda ()
         (for/list ([f (in-list f)])
           (dynamic-require
            `(file ,(if (list? f) (car f) f))
            (void))))
       null))
    (log-cover-benchmark-info "compile: ~a" (list t1 t2 t3))))


(define (test-files f)
  (define-values (_ t1 t2 t3)
    (time-apply
     (lambda ()
       (for ([f (in-list f)])
         (run-file!
          (if (list? f) (car f) f)
          'test
          (if (list? f) (cadr f) #()))))
     null))
  (log-cover-benchmark-info "run: ~a" (list t1 t2 t3)))