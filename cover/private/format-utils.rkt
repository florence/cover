#lang racket/base
(provide make-covered? irrelevant-submodules)
(require racket/file
         racket/function
         racket/list
         racket/match
         racket/port
         racket/set
         racket/bool

         syntax-color/racket-lexer
         syntax-color/lexer-contract
         syntax/modread
         syntax/parse

         data/interval-map

         "shared.rkt")

(module+ test (require rackunit racket/runtime-path racket/set))

;;;;; a Coverage is the output of (hash-of any (listof (list boolean srcloc?)))

;;;;; utils

;;; a Cover is (U 'covered 'uncovered 'irrelevant)

;; Coverage Any -> [Nat -> Cover]
(define (make-covered? coverage key)
  (unless (hash-has-key? coverage key)
    (error 'cover "no coverage information for ~s" key))
  (define c (hash-ref coverage key))
  (define submods (irrelevant-submodules))
  (define file-location-coverage-cache
    (coverage-cache-file key c submods))
  (lambda (loc)
    (interval-map-ref file-location-coverage-cache loc 'irrelevant)))

;; (or/c #f (listof symbol))
(define irrelevant-submodules (make-parameter #f))
(define current-file (make-parameter #f))

;; Path FileCoverage -> [Hashof Natural Cover]
;; build a hash caching coverage info for that file
(define (coverage-cache-file key c submods)
  (parameterize ([current-file key] [port-count-lines-enabled #t])
    (log-cover-debug "caching coverage info for ~s\n")
    (define get-covered (raw-covered c))

    (when (path-string? key)
      (call-with-input-file key
        (lambda (input)
          (define lexer
            (maybe-wrap-lexer
             (with-handlers ([exn:fail:read? (const racket-lexer)])
               (define f (read-language input))
               (cond [f (f 'color-lexer racket-lexer)]
                     [else racket-lexer]))))
          (make-irrelevant! lexer key input submods get-covered))))

    get-covered))

;; There are two variatese of racket lexers
;; if we are given the one argument kind, wrap it to the three arg kind
(define (maybe-wrap-lexer lexer)
  (if (procedure-arity-includes? lexer 3)
      lexer
      (λ (in offset mode)
        (define-values (a b c d e) (lexer in))
        (values a b c d e 0 #f))))

;; Lexer(see color:text<%>) FileName InputPort (Maybe (Listof Symbol)) CoverageIntervalMap
;;   -> Void
;; builds a function that determines if a given location in that port is irrelivent.
(define (make-irrelevant! lexer file input submods cmap)
  (define str (port->string input))
  (define fstr (file->string file))

  (lex-irrelevant! lexer fstr str cmap)
  (submod-irrelevant! fstr submods cmap))

;; Lexer String String (-> Natural Natural) Interval-Map -> Void
;; make comments irrelevant
(define (lex-irrelevant! lexer fstr str cmap)
  (define init-offset (- (string-length fstr) (string-length str)))
  (define for-lex (open-input-string str))
  (port-count-lines! for-lex)

  (let loop ([mode #f])
    (define-values (v type _m start end backup-dist new-mode/ds)
      (lexer for-lex 0 mode))
    (define new-mode
      (if (dont-stop? new-mode/ds)
          (dont-stop-val new-mode/ds)
          new-mode/ds))
    (case type
      [(eof) (void)]
      [(comment sexp-comment white-space)
       (define s (+ init-offset start))
       (define e (+ init-offset end))
       (update-map! cmap s e 'irrelevant)
       (loop new-mode)]
      [else (loop new-mode)])))

;; String (Maybe (Listof Symbol)) (-> Natural Natural) Interval-Map -> Void
;; make listed submodules irrelevant
(define (submod-irrelevant! str submods cmap)
  ;; stx positions are in terms of bytes
  (define stx
    (with-input-from-string str
      (thunk (with-module-reading-parameterization read-syntax))))

  (let loop ([stx stx] [first? #t])
    (define (loop* stx) (loop stx #f))
    (syntax-parse stx
      #:datum-literals (module module* module+ begin-for-syntax)
      [((~or module module* module+ begin-for-syntax)
        n:id
        e ...)
       #:when (and (not first?)
                   (implies submods (member (syntax-e #'n) submods)))
       (define ?start (syntax-position stx))
       (when ?start
         (define start ?start)
         (define end (+ ?start (syntax-span stx)))
         (update-map! cmap start end 'irrelevant))]
      [(e ...) (for-each loop* (syntax->list #'(e ...)))]
      [_else (void)])))

;; Coverage -> (IntervalMap (U 'covered 'uncovered 'irrelevant))
;; create map for looking up coverage information. irrelevant if its not contained
;; this code assumes that if two expression ranges overlap, then one is completely
;; contained within the other.
(define (raw-covered c)
  (define ordered (sort c srcloc<= #:key second))
  (define r (make-interval-map))

  (for ([pair (in-list ordered)])
    (match-define (list m (srcloc _ _ _ start range)) pair)
    (define val (if m 'covered 'uncovered))
    (update-map! r start (+ start range) val))

  r)

;; srcloc srcloc -> bool
;; based on start pos, with fallback to range
(define (srcloc<= locl locr)
  (match-define (srcloc _ _ _ startl rangel) locl)
  (match-define (srcloc _ _ _ startr ranger) locr)
  (or (< startl startr)
      (and (= startl startr)
           (<= ranger rangel))))

;; intervalmap nat nat any file -> void
;; sets the interval map if the range makes sense
;; logs a warning otherwise
(define (update-map! i s e v [extra-debug #f])
  (if (> e s)
      (interval-map-set! i s e v)
      (log-message
       (current-logger)
       'warning
       'cover
       (format "found non-sensable character range [~a,~a) in file ~a. Skiping coverage info for that range ~a"
               s e (current-file) (or extra-debug ""))
       (current-continuation-marks))))

(module+ test
  (require racket/lazy-require)
  (lazy-require ["../cover.rkt"
                 (make-cover-environment
                  test-files!
                  get-test-coverage)])
  (define-runtime-path cover.rkt "../cover.rkt")
  (define current-cover-environment
    (dynamic-require cover.rkt 'current-cover-environment))
  (define-runtime-path path2 "../tests/prog.rkt")
  (parameterize ([irrelevant-submodules #f])
    (test-begin
     (parameterize ([current-cover-environment (make-cover-environment)])
       (define f (path->string (simplify-path path2)))
       (test-files! f)
       (define coverage (get-test-coverage))
       (define covered? (curry coverage f))
       (check-equal? (covered? 14) 'irrelevant)
       (check-equal? (covered? 17) 'irrelevant)
       (check-equal? (covered? 28) 'irrelevant)
       (check-equal? (covered? 35) 'covered)
       (check-equal? (covered? 52) 'irrelevant)
       (check-equal? (covered? 53) 'irrelevant)
       (check-equal? (covered? 54) 'irrelevant)
       (check-equal? (covered? 50) 'uncovered)
       (check-equal? (covered? 78) 'uncovered)
       (check-equal? (covered? 106) 'uncovered)))))
