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
         racket/bool
         racket/set
         racket/list
         "private/file-utils.rkt"
         "private/shared.rkt")

;; Constant bits of stx to get all of covers names to have the same context
(define i* (make-syntax-introducer 'add))
(define #%require-name (i* #'#%require))
(define log-message-name (i* #'log-message))
(define current-logger-name (i* #'current-logger))
(define unsafe-vector*-set!-name (i* #'unsafe-vector*-set!))
(define unsafe-vector*-ref-name (i* #'unsafe-vector*-ref))
(define vector-name (i* #'cover-coverage-vector))
(define make-log-receiver-name (i* #'make-log-receiver))
(define sync-name (i* #'sync))
(define app-name (i* #'#%app))
(define hash-ref-name (i* #'hash-ref))
(define bfs-name (i* #'begin-for-syntax))
(define begin-name (i* #'begin))
(define quote-name (i* #'quote))
(define define-values-name (i* #'define-values))

;; symbol [Hash srcloclist index] [Hash pathstring vector] (Setof String)
;; -> (pathstring [(Setof String)] -> (Syntax -> Syntax))
(define ((make-annotate-top topic file->loc->vecref vecmapping) file [live-files-set #f])
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
               ;; are not annotatable, but can be concidered completely covered
               (define loc (stx->srcloc stx))
               (when loc
                 (initialize-test-coverage-point stx loc))
               (do-final-init! #t)
               stx]
              [else
               (define top (annotate-top stx phase))
               (do-final-init!)
               (define r (add-cover-require (annotate-clean top) file topic))
               (or r stx)]))
      e))

  (define (do-final-init! [value #f])
    (cond
      [(not initialized?)
       (hash-set! vecmapping file (make-vector count value))]
      [(not (zero? count))
       (define old (hash-ref vecmapping file))
       (define new-vector (make-vector (+ count (vector-length old)) value))
       (vector-copy! new-vector 0 old)
       (hash-set! vecmapping file new-vector)]
      [else (void)]))

  ;; in order to write modules to disk the top level needs to
  ;; be a module. so we trust that the module is loaded and trim the expression
  (define (annotate-clean e)
    (kernel-syntax-case
     e #f
     [(begin e mod)
      (begin
        (syntax-case #'e (quote)
          [(_ #;#%app vector-set vec (quote loc) (quote #t))
           (vector-set! (hash-ref vecmapping file) (syntax-e #'loc) #t)])
        #'mod)]
     [_ e]))

  (define (initialize-test-coverage-point stx loc)
    (unless (hash-has-key? loc->vecref loc)
      (hash-set! loc->vecref loc (list file count))
      (set! count (add1 count))))

  (define (test-covered stx loc)
    (with-syntax ([vector-name vector-name]
                  [#%papp app-name]
                  [quote quote-name]
                  [unsafe-vector*-set! unsafe-vector*-set!-name]
                  [vecloc (second (hash-ref loc->vecref loc))])
      #`(#%papp unsafe-vector*-set! vector-name 'vecloc '#t)))

  (define (test-coverage-point body expr phase)
    (define loc (stx->srcloc expr))
    (cond [(and loc (implies live-files-set (set-member? live-files-set (first loc))))
           (initialize-test-coverage-point expr loc)
           #`(#,begin-name #,(test-covered expr loc) #,body)]
          [else body]))

  ;; ---- IN ----
  (define-values/invoke-unit/infer stacktrace/annotator@)
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
(define (get-syntax-depth expr [phase 0])
  (kernel-syntax-case/phase
   (disarm expr) phase
   [(module _ _ mb)
    (get-syntax-depth #'mb)]
   [(module* _ _ mb)
    (get-syntax-depth #'mb)]
   [(begin-for-syntax b ...)
    (add1 (apply max 1 (for/list ([b (in-list (syntax->list #'(b ...)))])
                         (get-syntax-depth b (add1 phase)))))]
   [(define-syntaxes a ...)
    2]
   [(b ...)
    (apply max 1 (for/list ([b (in-list (syntax->list #'(b ...)))])
                   (get-syntax-depth b phase)))]
   [_ 1]))

;; Natural PathString Symbol -> Syntax
;; Build a set of requires and definitions for cover to insert
(define (build-adds bfs-depth file topic)
  (with-syntax ([log-message log-message-name]
                [current-logger current-logger-name]
                [unsafe-vector*-ref unsafe-vector*-ref-name]
                [vector-name vector-name]
                [make-log-receiver make-log-receiver-name]
                [sync sync-name]
                [file file]
                [hash-ref hash-ref-name]
                [#%papp app-name]
                [pdefine-values define-values-name]
                [pbegin begin-name]
                [prequire #%require-name]
                [pbegin-for-syntax bfs-name]
                [send-name (format-symbol "~a~a" topic 'cover-internal-send-vector-mapping)]
                [req-name (format-symbol "~a~a" topic 'cover-internal-request-vector-mapping)]
                [quote quote-name])
    (define startup-code
      #`(pbegin
          (pdefine-values (lgr) (#%papp current-logger))
          (pdefine-values (rec) (#%papp make-log-receiver lgr 'info 'send-name))
          (pdefine-values (vector-name)
                          (pbegin
                           (#%papp log-message lgr 'info 'req-name '"" '#f)
                           (#%papp hash-ref
                                   (#%papp unsafe-vector*-ref
                                           (#%papp sync rec)
                                           '2)
                                   'file)))))
    #`(#,@(for/list ([i bfs-depth])
            #`(prequire (for-meta #,i (only '#%kernel quote))
                        (for-meta #,i (rename '#%kernel log-message log-message))
                        (for-meta #,i (rename '#%kernel pbegin-for-syntax begin-for-syntax))
                        (for-meta #,i (rename '#%kernel current-logger current-logger))
                        (for-meta #,i (rename '#%kernel make-log-receiver make-log-receiver))
                        (for-meta #,i (rename '#%kernel sync sync))
                        (for-meta #,i (rename '#%kernel hash-ref hash-ref))
                        (for-meta #,i (rename '#%kernel #%papp #%app))
                        (for-meta #,i (rename '#%kernel pdefine-values define-values))
                        (for-meta #,i (rename '#%kernel pbegin begin))
                        (for-meta #,i (rename '#%unsafe unsafe-vector*-ref unsafe-vector*-ref))
                        (for-meta #,i (rename '#%unsafe unsafe-vector*-set! unsafe-vector*-set!))))
       (prequire (for-meta #,bfs-depth (rename '#%kernel pbegin begin)))
       #,(for/fold ([stx #'(pbegin)]) ([i bfs-depth])
           #`(pbegin #,startup-code (pbegin-for-syntax #,stx))))))

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
(define profile-key (gensym))
(define profiling-enabled (make-parameter #f))
(define initialize-profile-point void)
(define (register-profile-start . a) #f)
(define register-profile-done void)
