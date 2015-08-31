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

;; Constant bits of stx to get all of covers names to have the same context
(define log-message-name #'log-message)
(define current-logger-name #'current-logger)
(define unsafe-vector-set!-name #'unsafe-vector*-set!)
(define unsafe-vector-ref-name #'unsafe-vector*-ref)
(define vector-name #'cover-coverage-vector)
(define make-log-receiver-name #'make-log-receiver)
(define sync-name #'sync)
(define hash-ref-name #'hash-ref)

;; symbol [Hash srcloclist index] [Hash pathstring vector]
;; -> (pathstring -> annotator)
(define ((make-annotate-top topic file->loc->vecref vecmapping) file)
  (define initialized? (hash-has-key? file->loc->vecref file))

  (unless initialized?
    (hash-set! file->loc->vecref file (make-hash)))

  (define loc->vecref (hash-ref file->loc->vecref file))
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
               (define r (add-cover-require (annotate-clean top) file topic))
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
            (hash-set! loc->vecref loc (list file count))
            (set! count (add1 count))))))

  (define (test-covered stx)
    (define loc (stx->srcloc stx))
    (with-syntax ([vector-name vector-name]
                  [unsafe-vector-set! unsafe-vector-set!-name]
                  [vecloc (cadr (hash-ref loc->vecref loc))])
      #`(#%plain-app unsafe-vector-set! vector-name vecloc #t)))

  ;; ---- IN ----
  (define-values/invoke-unit/infer stacktrace@)
  (make-cover-annotate-top annotate-top))

;;  -------- Annotation Helpers --------------

(define (cross-phase-persist? stx)
  (define disarmed (disarm stx))
  (kernel-syntax-case
   disarmed #f
   [(module name lang (#%module-begin e ...))
    (member '(#%declare #:cross-phase-persistent) (syntax->datum #'(e ...)))
    #t]
   [_ #f]))

;; Syntax PathString Symbol -> Syntax
;; This function inserts the necessary requires and definitions for cover to run
;; properly. It only touches begins, begin-for-syntaxes, and submodules. Everything
;; else should be ignored.
(define (add-cover-require expr file topic)
  (define bfs-depth (get-syntax-depth expr))

  (define/with-syntax (add ...)
    (build-adds bfs-depth file topic))

  ;; -- IN --

  (let loop ([expr expr] [phase 0] [top #t])
    (define disarmed (disarm expr))
    (kernel-syntax-case
     disarmed #f

     [(m name lang mb)
      (or (eq? 'module (syntax-e #'m))
          (eq? 'module* (syntax-e #'m)))
      (let ()
        (define lexical? (eq? #f (syntax-e #'lang)))
        ;; When we enter a lexically scoped submodule we must shift its
        ;; phase to 0, then back again after we annotate
        (define phase-shift (if lexical? phase 0))
        (define shifted (syntax-shift-phase-level disarmed (- phase-shift)))
        (syntax-case shifted ()
          [(m name lang mb)
           (syntax-case (syntax-disarm #'mb inspector) ()
             [(#%module-begin b ...)
              (let ()
                (define/with-syntax (body ...)
                  (map (lambda (e) (loop e 0 #f))
                       (syntax->list #'(b ...))))
                (define stx
                  #`(m name lang
                       (#%module-begin add ... body ...)))
                (rebuild-syntax stx disarmed expr phase-shift))])]))]

     [(b a ...)
      (or (eq? 'begin (syntax-e #'b))
          (eq? 'begin-for-syntax (syntax-e #'b)))
      (let ()
        (define new-phase
          (if (eq? 'begin-for-syntax (syntax-e #'b))
              (add1 phase)
              phase))
        (define/with-syntax (body ...)
          (map (lambda (e) (loop e new-phase #f))
               (syntax->list #'(a ...))))
        #'(b body ...))]

     [_ (if top #f expr)])))

;; Syntax -> Natural
;; Maxiumum depth of begin-for-syntaxes
(define (get-syntax-depth expr)
  (kernel-syntax-case
   (disarm expr) #f
   [(module _ _ mb)
    (get-syntax-depth #'mb)]
   [(module* _ _ mb)
    (get-syntax-depth #'mb)]
   [(begin-for-syntax b ...)
    (add1 (apply max 1 (map get-syntax-depth (syntax->list #'(b ...)))))]
   [(b ...)
    (apply max 1 (map get-syntax-depth (syntax->list #'(b ...))))]
   [_ 1]))

;; Natural PathString Symbol -> Syntax
;; Build a set of requires and definitions for cover to insert
(define (build-adds bfs-depth file topic)
  (with-syntax ([log-message log-message-name]
                [current-logger current-logger-name]
                [unsafe-vector-set! unsafe-vector-set!-name]
                [unsafe-vector-ref unsafe-vector-ref-name]
                [vector-name vector-name]
                [make-log-receiver make-log-receiver-name]
                [sync sync-name]
                [file file]
                [hash-ref hash-ref-name]
                [#%papp #'#%app]
                [pdefine-values #'define-values]
                [pbegin #'begin]
                [prequire '#%require]
                [send-name (format-symbol "~a~a" topic 'cover-internal-send-vector-mapping)]
                [req-name (format-symbol "~a~a" topic 'cover-internal-request-vector-mapping)])
    #`(#,@(for/list ([i bfs-depth])
            #`(#%require (for-meta #,i (rename '#%kernel prequire #%require))))
       #,@(for/list ([i bfs-depth])
            #`(prequire (only '#%kernel quote)
                        (for-meta #,i (rename '#%kernel log-message log-message))
                        (for-meta #,i (rename '#%kernel current-logger current-logger))
                        (for-meta #,i (rename '#%kernel make-log-receiver make-log-receiver))
                        (for-meta #,i (rename '#%kernel sync sync))
                        (for-meta #,i (rename '#%kernel hash-ref hash-ref))
                        (for-meta #,i (rename '#%kernel #%papp #%app))
                        (for-meta #,i (rename '#%kernel pdefine-values define-values))
                        (for-meta #,i (rename '#%kernel pbegin begin))
                        (for-meta #,i (rename '#%unsafe unsafe-vector-ref unsafe-vector-ref))
                        (for-meta #,i (rename '#%unsafe unsafe-vector-set! unsafe-vector-set!))))
       (pdefine-values (lgr) (#%papp current-logger))
       (pdefine-values (rec) (#%papp make-log-receiver lgr 'info 'send-name))
       (pdefine-values (vector-name)
                       (pbegin
                        (#%papp log-message lgr 'info 'req-name "" #f)
                        (#%papp hash-ref
                                (#%papp unsafe-vector-ref
                                        (#%papp sync rec)
                                        2)
                                file))))))

(define inspector (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

(define (disarm stx)
  (syntax-disarm stx inspector))

(define (stx->srcloc stx)
  (and (syntax? stx)
       (let* ([orig-src (syntax-source stx)]
              [src (if (path? orig-src) (path->string orig-src) orig-src)]
              [pos (syntax-position stx)]
              [span (syntax-span stx)])
         (and pos
              span
              (list src #f #f pos span)))))

(define (rebuild-syntax stx disarmed armed phase)
  (syntax-rearm
   (syntax-shift-phase-level
    (datum->syntax
     disarmed
     (syntax-e stx)
     disarmed
     disarmed)
    phase)
   armed))

;;  -------- Generic `stacktrace^` Imports --------------
(define (with-mark src dest phase) dest)
(define test-coverage-enabled (make-parameter #t))
(define profile-key (gensym))
(define profiling-enabled (make-parameter #f))
(define initialize-profile-point void)
(define (register-profile-start . a) #f)
(define register-profile-done void)
