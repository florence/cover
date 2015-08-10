#lang racket/base
(provide make-annotate-top)

#|
The module implements code coverage annotations as described in cover.rkt
|#

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

;; symbol [Hash srcloclist index] [Hash pathstring vector]
;; -> (pathstring -> annotator)
(define (make-annotate-top topic loc->vecref vecmapping)
  (define log-message-name #'log-message)
  (define current-logger-name #'current-logger)
  (define unsafe-vector-set!-name #'unsafe-vector*-set!)
  (define unsafe-vector-ref-name #'unsafe-vector*-ref)
  (define vector-name #'cover-coverage-vector)
  (define make-log-receiver-name #'make-log-receiver)
  (define sync-name #'sync)
  (define hash-ref-name #'hash-ref)

  (define (cross-phase-persist? stx)
    (define disarmed (disarm stx))
    (kernel-syntax-case
     disarmed #f
     [(module name lang (#%module-begin e ...))
      (member '(#%declare #:cross-phase-persistent) (syntax->datum #'(e ...)))
      #t]
     [_ #f]))

  (define (add-cover-require expr file)
    (let loop ([expr expr] [top #t])
      (define disarmed (disarm expr))
      (kernel-syntax-case
       disarmed #f
       [(m name lang mb)
        (or (eq? 'module (syntax-e #'m))
            (eq? 'module* (syntax-e #'m)))
        (with-syntax ([log-message log-message-name]
                      [current-logger current-logger-name]
                      [unsafe-vector-set! unsafe-vector-set!-name]
                      [unsafe-vector-ref unsafe-vector-ref-name]
                      [vector-name vector-name]
                      [make-log-receiver make-log-receiver-name]
                      [sync sync-name]
                      [file file]
                      [hash-ref hash-ref-name]
                      [send-name (format-symbol "~a~a" topic 'cover-internal-send-vector-mapping)]
                      [req-name (format-symbol "~a~a" topic 'cover-internal-request-vector-mapping)])
          (define lexical? (eq? #f (syntax-e #'lang)))
          (syntax-case (syntax-disarm #'mb inspector) ()
            [(#%module-begin b ...)
             (let ()
               (define/with-syntax (body ...)
                 (map (lambda (e) (loop e #f))
                      (syntax->list #'(b ...))))
               (define/with-syntax (add ...)
                 #'((#%require (rename '#%kernel log-message log-message)
                               (rename '#%kernel current-logger current-logger)
                               (rename '#%kernel make-log-receiver make-log-receiver)
                               (rename '#%kernel sync sync)
                               (rename '#%kernel hash-ref hash-ref)
                               (only '#%kernel #%app define-values printf)
                               (rename '#%unsafe unsafe-vector-ref unsafe-vector-ref)
                               (rename '#%unsafe unsafe-vector-set! unsafe-vector-set!))
                    (define-values (lgr) (#%app current-logger))
                    (define-values (rec)
                      (#%app make-log-receiver
                             lgr
                             'info
                             'send-name))
                    (define-values (vector-name)
                      (begin
                        (#%app log-message
                               lgr
                               'info
                               'req-name
                               ""
                               #f)
                        (#%app
                         hash-ref
                         (#%app
                          unsafe-vector-ref
                          (#%app sync rec)
                          2)
                         file)))))
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


  (lambda (file)
    (define initialized? (hash-has-key? loc->vecref file))
    (define count 0)

    (define (make-cover-annotate-top annotate-top)
    (lambda (stx phase)
      (define e
        (cond [(cross-phase-persist? stx)
               ;; special case: cross-phase-pesistant files
               ;; are not coverable, but immutable so basically always covered
               (initialize-test-coverage-point stx)
               (do-final-init! #t)
               stx]
              [else
               (define top (annotate-top stx phase))
               (do-final-init!)
               (define r (add-cover-require (annotate-clean top) file))
               (or r stx)]))
      e))

    (define (do-final-init! [value #f])
      (unless initialized?
        (hash-set! vecmapping file (make-vector count value))))

  ;; in order to write modules to disk the top level needs to
  ;; be a module. so we trust that the module is loaded and trim the expression
  (define (annotate-clean e)
    (kernel-syntax-case
     e #f
     [(begin e mod)
      (begin
        (syntax-case #'e (#%plain-app)
          [(#%plain-app vector-set vec loc #t)
           (vector-set! (hash-ref vecmapping file) (syntax-e #'loc) #t)])
        #'mod)]
     [_ e]))

    (define initialize-test-coverage-point
      (if initialized?
          void
          (lambda (stx)
            (define loc (stx->srcloc stx))
            (unless (hash-has-key? loc->vecref loc)
              (hash-set! loc->vecref loc count)
              (set! count (add1 count))))))

    (define (test-covered stx)
      (define loc (stx->srcloc stx))
      (with-syntax ([vector-name vector-name]
                    [unsafe-vector-set! unsafe-vector-set!-name]
                    [vecloc (hash-ref loc->vecref loc)])
        #`(#%plain-app unsafe-vector-set! vector-name vecloc #t)))

    ;; ---- IN ----
    (define-values/invoke-unit/infer stacktrace@)
    (make-cover-annotate-top annotate-top)))


(require racket/pretty)



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
