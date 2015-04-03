#lang racket/base
(provide make-annotate-top)
(require errortrace/stacktrace
         racket/function
         racket/syntax
         syntax/parse
         racket/unit
         syntax/kerncase
         racket/runtime-path
         racket/syntax
         "private/file-utils.rkt"
         "private/shared.rkt")


(define (make-annotate-top c cover-name)
  (define lift-name #'do-lift)
  (define set-box-name #'set-box!)
  (define hash-ref-name #'hash-ref)

  ;;  -------- Specific `stacktrace^` Imports --------------

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
      #`(#%plain-app set-box! (do-lift (#%plain-app hash-ref c loc)) #t)))


  ;;  -------- Cover's Specific Annotators --------------
  (define (make-cover-annotate-top annotate-top)
    (lambda (stx phase)
      (define e (add-cover-require stx))
      (if e (expand-syntax (annotate-clean (annotate-top (expand-syntax e) phase))) stx)))

  (define (add-cover-require expr)
    (define inspector (variable-reference->module-declaration-inspector
                       (#%variable-reference)))
    (let loop ([expr expr] [top #t])
      (define disarmed (syntax-disarm expr inspector))
      (kernel-syntax-case
       disarmed #f
       [(module name lang (#%module-begin e ...))
        (member '(#%declare #:cross-phase-persistent) (syntax->datum #'(e ...)))
        #f]
       [(m name lang mb)
        (or (eq? 'module (syntax-e #'m))
            (eq? 'module* (syntax-e #'m)))
        (with-syntax ([cover cover-name]
                      [set-box set-box-name]
                      [hash-rf hash-ref-name]
                      [do-lift lift-name])
          (define lexical? (eq? #f (syntax-e #'lang)))
          (syntax-case (syntax-disarm #'mb inspector) ()
            [(#%module-begin b ...)
             (let ()
               (define/with-syntax (body ...)
                 (map (lambda (e) (loop e #f))
                      (syntax->list #'(b ...))))
               (define/with-syntax (add ...)
                 #'((#%require (rename cover/coverage cover coverage)
                               (rename '#%kernel set-box set-box!)
                               (rename '#%kernel haah-rf hash-ref))
                    (#%require (for-syntax '#%kernel))
                    (define-syntaxes (do-lift)
                      (lambda (stx)
                        (syntax-local-lift-expression
                         (cadr (syntax-e stx)))))))
               (define stx
                 #'(m name lang
                      (#%module-begin add ... body ...)))
               (rebuild-syntax stx disarmed expr))]))]
       [_ (if top #f expr)])))
  ;; in order to write modules to disk the top level needs to
  ;; be a module. so we trust that the module is loaded and trim the expression
  (define (annotate-clean e)
    (kernel-syntax-case
     e #f
     [(begin e mod)
      (begin
        (syntax-case #'e (#%plain-app set-box! do-lift make-srcloc hash-ref)
          [(#%plain-app set-box! (lift (#%plain-app hash-ref _ (quote v))) _)
           (let ([location (syntax->datum #'v)])
             (set-box! (hash-ref c location) #t))])
        #'mod)]
     [_ e]))


  ;; ---- IN ----
  (define-values/invoke-unit/infer stacktrace@)
  (make-cover-annotate-top annotate-top))





;;  -------- Generic `stacktrace^` Imports --------------
(define (with-mark src dest phase) dest)
(define test-coverage-enabled (make-parameter #t))
(define profile-key (gensym))
(define profiling-enabled (make-parameter #f))
(define initialize-profile-point void)
(define (register-profile-start . a) #f)
(define register-profile-done void)

;;  -------- Annotation Helpers --------------

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

(define (rebuild-syntax stx disarmed armed)
  (syntax-rearm
   (namespace-syntax-introduce
    (datum->syntax
     disarmed
     (syntax-e stx)
     disarmed
     disarmed))
   armed))
