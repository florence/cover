#lang racket/base
(provide (rename-out [in:annotate-top annotate-top]))
(require errortrace/stacktrace
         racket/function
         racket/syntax
         syntax/parse
         racket/unit
         racket/runtime-path
         "coverage.rkt")

(define cover-name (generate-temporary #'coverage))
(define srcloc-name (generate-temporary #'make-srcloc))

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
  (define e
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      [((~and a module) name lang b ...)
       (with-syntax ([cover cover-name] [srcloc srcloc-name])
         (namespace-syntax-introduce
          #'(a name lang
              (#%require (rename cover/coverage cover coverage))
              (#%require (rename racket/base srcloc make-srcloc))
              r1 r2
              b ...)))]
      [_ #f]))
  (if e (annotate-top e phase) stx))
