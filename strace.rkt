#lang racket/base
(provide make-annotate-top)
(require errortrace/stacktrace
         racket/function
         racket/syntax
         syntax/parse
         racket/unit
         syntax/kerncase
         racket/runtime-path
         racket/fixnum
         "private/file-utils.rkt"
         "private/shared.rkt")

(define (make-annotate-top c cover-name)
  (define (with-mark src dest phase) dest)
  (define test-coverage-enabled (make-parameter #t))

  (define lift-name #'do-lift)
  (define set-box-name #'set-box!)
  (define hash-ref-name #'hash-ref)


  (define profile-key (gensym))

  (define profiling-enabled (make-parameter #f))
  (define initialize-profile-point void)
  (define (register-profile-start . a) #f)
  (define register-profile-done void)
  (define (initialize-test-coverage-point stx)
    (define srcloc (stx->srcloc stx))
    (when srcloc
      (hash-set! c srcloc (box #f))))

  (define (test-covered stx)
    (define loc/stx (stx->srcloc/stx stx))
    (with-syntax ([c cover-name]
                  [loc loc/stx]
                  [set-box! set-box-name]
                  [hash-ref hash-ref-name]
                  [do-lift lift-name])
      #`(set-box! (do-lift (hash-ref c loc)) #t)))


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

  (define o (current-output-port))
  (define (in:annotate-top annotate-top)
    (lambda (stx phase)
      (define e (add-cover-require stx))
      (let ([tmp (if e (expand-syntax (annotate-clean (annotate-top (expand-syntax e) phase))) stx)])
        #;
        (when (equal? (string->path "/Users/florence/playground/cover/private/format-utils.rkt") (syntax-source e))
          (define ce (dynamic-require 'racket/gui 'current-eventspace))
          (define me (dynamic-require 'racket/gui 'make-eventspace))
          (parameterize ([ce (me)])
            (thread (lambda () ((dynamic-require 'macro-debugger/syntax-browser 'browse-syntax) tmp))))
          (let loop () (loop)))
        ;    (write (syntax-source e) o)
        ;    (displayln "")
        tmp)))

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
                           (with-syntax ([cover cover-name]
                                         [set-box set-box-name]
                                         [hash-rf hash-ref-name]
                                         [do-lift lift-name])
                             (syntax-case (syntax-disarm #'mb inspector) ()
                               [(#%module-begin b ...)
                                (with-syntax ([(body ...)
                                               (map (lambda (e) (loop e #f)) (syntax->list #'(b ...)))])
                                  (syntax-rearm
                                   (namespace-syntax-introduce
                                    (datum->syntax
                                     disarmed
                                     (syntax-e
                                      #'(m name lang
                                           (#%module-begin
                                            (#%require (rename cover/coverage cover coverage)
                                                       (rename '#%kernel set-box set-box!)
                                                       (rename '#%kernel hash-rf hash-ref))
                                            (#%require (for-syntax '#%kernel))
                                            (define-syntaxes (do-lift)
                                              (lambda (stx)
                                                (syntax-local-lift-expression (cadr (syntax-e stx)))))
                                            body ...)))
                                     disarmed
                                     disarmed))
                                   expr))]))]
                          [_ (if top #f expr)])))

  ;; in order to write modules to disk the top level needs to
  ;; be a module. so we trust that the module is loaded and trim the expression
  (define (annotate-clean e)
  (kernel-syntax-case e #f
    [(begin e mod)
     (begin
       (syntax-case #'e (set-box! do-lift make-srcloc hash-ref)
         [(set-box! (lift (hash-ref _ (make-srcloc v ...))) _)
          (let ([location (apply make-srcloc (syntax->datum #'(v ...)))])
            (set-box! (hash-ref c location) #t))])
       #'mod)]
    [_ e]))

  (define-values/invoke-unit/infer stacktrace@)
  (in:annotate-top annotate-top))
