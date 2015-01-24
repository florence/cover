#lang racket/base
(provide (rename-out [in:annotate-top annotate-top]))
(require errortrace/stacktrace
         racket/function
         racket/syntax
         syntax/parse
         racket/unit
         racket/runtime-path
         "private/file-utils.rkt"
         "private/shared.rkt"
         "coverage.rkt")

(define cover-name #'coverage)
(define srcloc-name  #'make-srcloc)

(define (with-mark src dest phase) dest)
(define test-coverage-enabled (make-parameter #t))

(define (initialize-test-coverage-point stx)
  (define srcloc (stx->srcloc stx))
  (when srcloc
    (hash-set! coverage srcloc #f)))

(define (test-covered stx)
  (define loc/stx (stx->srcloc/stx stx))
  (with-syntax ([c cover-name]
                [loc loc/stx])
    #'(#%plain-app hash-set! c loc #t)))

(define profile-key (gensym))

(define profiling-enabled (make-parameter #f))
(define initialize-profile-point void)
(define (register-profile-start . a) #f)
(define register-profile-done void)

(define-values/invoke-unit/infer stacktrace@)

(define (make-srcloc-maker f)
  (lambda (stx)
    (and (syntax? stx)
         (let* ([orig-src (syntax-source stx)]
                [src (if (path? orig-src) (path->string orig-src) orig-src)]
                [pos (syntax-position stx)]
                [span (syntax-span stx)])
           (and pos
                span
                (f src #f #f pos span))))))

(define stx->srcloc
  (make-srcloc-maker make-srcloc))

(define stx->srcloc/stx
  (make-srcloc-maker
   (lambda (src a b pos span)
     (with-syntax ([src src]
                   [pos pos]
                   [a a]
                   [b b]
                   [span span]
                   [make-srcloc srcloc-name])
       #'(make-srcloc src a b pos span)))))

(define (in:annotate-top stx phase)
  (define e (add-cover-require stx))
  (if e (annotate-clean (annotate-top e phase)) stx))

(define (add-cover-require expr)
  (define inspector (variable-reference->module-declaration-inspector
                     (#%variable-reference)))
  (let loop ([expr expr] [top #t])
    (syntax-parse (syntax-disarm expr inspector)
      #:literal-sets (kernel-literals)
      [(module name lang mb)
       (with-syntax ([cover cover-name]
                     [srcloc srcloc-name])
         (syntax-parse (syntax-disarm #'mb inspector)
           [(#%module-begin b ...)
            (with-syntax ([(body ...)
                           (map (lambda (e) (loop e #f)) (syntax->list #'(b ...)))])
              (syntax-rearm
               (namespace-syntax-introduce
                (quasisyntax/loc expr
                  (module name lang
                    (#%module-begin
                     (#%require (rename cover/coverage cover coverage))
                     (#%require (rename racket/base srcloc make-srcloc))
                     body ...))))
               expr))]))]
      [_ (if top #f expr)])))

;; in order to write modules to disk the top level needs to
;; be a module. so we trust that the module is loaded and trim the expression
(define (annotate-clean e)
  (syntax-parse e
    #:literal-sets (kernel-literals)
    [(begin e mod)
     (eval #'e)
     #'mod]
    [_ e]))
