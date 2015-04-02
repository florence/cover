#lang racket/base
(provide make-annotate-top)
(require errortrace/stacktrace
         racket/function
         racket/syntax
         racket/unit
         syntax/kerncase
         racket/runtime-path
         "private/file-utils.rkt"
         "private/shared.rkt")

(define (make-annotate-top c cover-name)
  (define (initialize-test-coverage-point stx)
    (define srcloc (stx->srcloc stx))
    (when srcloc
      (hash-set! c srcloc #f)))
  (define (with-mark src dest phase) dest)
  (define test-coverage-enabled (make-parameter #t))

  (define (test-covered stx)
    (with-syntax ([c cover-name]
                  [loc (stx->srcloc/stx stx)])
      #'(#%plain-app hash-set! c loc #t)))

  (define profile-key (gensym))

  (define profiling-enabled (make-parameter #f))
  (define initialize-profile-point void)
  (define (register-profile-start . a) #f)
  (define register-profile-done void)


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
    (make-srcloc-maker list))

  (define stx->srcloc/stx
    (make-srcloc-maker
     (lambda (src a b pos span)
       (with-syntax ([src src]
                     [pos pos]
                     [a a]
                     [b b]
                     [span span])
         #'(quote (src a b pos span))))))

  (define (in:annotate-top annotate-top)
    (lambda (stx phase)
      (define e (add-cover-require stx))
      (if e (annotate-clean (annotate-top e phase)) stx)))

  (define (add-cover-require expr)
    (define inspector (variable-reference->module-declaration-inspector
                       (#%variable-reference)))
    (let loop ([expr expr] [top #t])
      (define disarmed (syntax-disarm expr inspector))
      (kernel-syntax-case disarmed #f
                          [(module name lang (#%module-begin e ...))
                           (member '(#%declare #:cross-phase-persistent) (syntax->datum #'(e ...)))
                           #f]
                          [(module name lang mb)
                           (with-syntax ([cover cover-name])
                             (syntax-case (syntax-disarm #'mb inspector) ()
                               [(#%module-begin b ...)
                                (with-syntax ([(body ...)
                                               (map (lambda (e) (loop e #f)) (syntax->list #'(b ...)))])
                                  (syntax-rearm
                                   (namespace-syntax-introduce
                                    (datum->syntax
                                     expr
                                     (syntax-e
                                      #'(module name lang
                                          (#%module-begin
                                           (#%require (rename cover/coverage cover coverage))
                                           body ...)))
                                     expr expr))
                                   expr))]))]
                          [_ (if top #f expr)])))

  ;; in order to write modules to disk the top level needs to
  ;; be a module. so we trust that the module is loaded and trim the expression
  (define (annotate-clean e)
    (kernel-syntax-case e #f
                        [(begin e mod)
                         (eval #'e)
                         #'mod]
                        [_ e]))

  (define-values/invoke-unit/infer stacktrace@)
  (in:annotate-top annotate-top))
