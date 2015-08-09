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

(define (make-annotate-top topic)
  (define log-message-name #'log-message)
  (define current-logger-name #'current-logger)

  ;;  -------- Specific `stacktrace^` Imports --------------

  (define (initialize-test-coverage-point stx)
    (define srcloc (stx->srcloc stx))
    (log-message (current-logger)
                 'info
                 topic
                 logger-init-message
                 srcloc #f))

  (define (test-covered stx)
    (define loc/stx (stx->srcloc/stx stx))
    (with-syntax ([current-logger current-logger-name]
                  [log-message log-message-name]
                  [loc loc/stx]
                  [logger-covered-message logger-covered-message])
      #`(#%plain-app log-message (current-logger)
                     'info '#,topic
                     logger-covered-message loc #f)))


  ;;  -------- Cover's Specific Annotators --------------
  (define (make-cover-annotate-top annotate-top)
    (lambda (stx phase)
      (define e
        (cond [(cross-phase-persist? stx)
               (initialize-test-coverage-point stx)
               (log-message (current-logger)
                            'info
                            topic
                            logger-covered-message
                            (stx->srcloc stx)
                            #f)
               stx]
              [(add-cover-require (annotate-clean (annotate-top stx phase)))
               => expand-syntax]
              [else stx]))
      e))

  (define (cross-phase-persist? stx)
    (define disarmed (disarm stx))
    (kernel-syntax-case
     disarmed #f
     [(module name lang (#%module-begin e ...))
      (member '(#%declare #:cross-phase-persistent) (syntax->datum #'(e ...)))
      #t]
     [_ #f]))

  (define (add-cover-require expr)
    (let loop ([expr expr] [top #t])
      (define disarmed (disarm expr))
      (kernel-syntax-case
       disarmed #f
       [(m name lang mb)
        (or (eq? 'module (syntax-e #'m))
            (eq? 'module* (syntax-e #'m)))
        (with-syntax ([log-message log-message-name]
                      [current-logger current-logger-name])
          (define lexical? (eq? #f (syntax-e #'lang)))
          (syntax-case (syntax-disarm #'mb inspector) ()
            [(#%module-begin b ...)
             (let ()
               (define/with-syntax (body ...)
                 (map (lambda (e) (loop e #f))
                      (syntax->list #'(b ...))))
               (define/with-syntax (add ...)
                 #'((#%require (rename '#%kernel log-message log-message)
                               (rename '#%kernel current-logger current-logger))))
               (define stx
                 #'(m name lang
                      (#%module-begin add ... body ...)))
               (rebuild-syntax stx disarmed expr))]))]
       [(b a ...)
        (eq? 'begin (syntax-e #'b))
        (let ()
          (define/with-syntax (body ...)
            (map (lambda (e) (loop e #f))
                 (syntax->list #'(a ...))))
          #'(b body ...))]
       [_ (if top #f expr)])))

  (define inspector (variable-reference->module-declaration-inspector
                     (#%variable-reference)))
  (define (disarm stx)
    (syntax-disarm stx inspector))
  ;; in order to write modules to disk the top level needs to
  ;; be a module. so we trust that the module is loaded and trim the expression
  (define (annotate-clean e)
    (kernel-syntax-case
     e #f
     [(begin e mod)
      (begin
        (syntax-case #'e (#%plain-app log-message)
          [(#%plain-app log-message _ _ _ "covered" (_ loc) #f)
           (log-message (current-logger) 'info topic "covered" (syntax->datum #'loc))])
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
   (datum->syntax
    disarmed
    (syntax-e stx)
    disarmed
    disarmed)
   armed))


#;
(module+ test
  (let ()
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (define ann (make-annotate-top))
      (define test
        (expand #'(module a racket 1)))
      (define r (make-log-receiver (current-logger) 'info logger-topic))
      (eval (ann test (namespace-base-phase ns)) ns)
      (eval '(require 'a) ns)
      (check-not-false (sync/timeout 0 r)))))
