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

;; Path FileCoverage -> [Hashof Natural Cover]
;; build a hash caching coverage info for that file
(define (coverage-cache-file key c submods)
  (vprintf "caching coverage info for ~s\n" key)
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

  get-covered)

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
(define (make-irrelevant! lexer f input submods cmap)
  (define-values (for-lex for-str) (replicate-file-port f input))
  (define str (apply vector (string->list (port->string for-str))))
  (define init-offset (- (string-length (file->string f))
                         (vector-length str)))

  (define offset (make-byte->str-offset str))

  ;; first do comments
  (let loop ([mode #f])
    (define-values (v type _m start end backup-dist new-mode/ds)
      (lexer for-lex 0 mode))
    (define new-mode (if (dont-stop? new-mode/ds)
                         (dont-stop-val new-mode/ds)
                         new-mode/ds))
    (case type
      [(eof) (void)]
      [(comment sexp-comment white-space)
       (define s (+ init-offset (- start (offset start))))
       (define e (+ init-offset (- end (offset end))))
       (interval-map-set! cmap s e 'irrelevant)
       (loop new-mode)]
      [else (loop new-mode)]))

  ;; then do submodules
  (define stx
    (with-input-from-file f
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
         (define start (- ?start (* 2 (offset ?start))))
         (define end (+ start (syntax-span stx)))
         (interval-map-set! cmap start end 'irrelevant))]
      [(e ...) (for-each loop* (syntax->list #'(e ...)))]
      [_else (void)])))

;; Path FilePort -> FilePort FilePort
;; creates two ports to that file at the same position at the first
(define (replicate-file-port f p)
  (define f1 (open-input-file f))
  (define f2 (open-input-file f))
  (file-position f1 (file-position p))
  (file-position f2 (file-position p))
  (values f1 f2))

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
    (interval-map-set! r start (+ start range) val))

  r)

(define (srcloc<= locl locr)
  (match-define (srcloc _ _ _ startl rangel) locl)
  (match-define (srcloc _ _ _ startr ranger) locr)
  (or (< startl startr)
      (and (= startl startr)
           (<= ranger rangel))))

;; String -> (Natural -> Natural)
;; used for determining character/byte offsets for a given
;; 1 indexed byte locaiton
(define (make-byte->str-offset str)
  (define lmapping
    (let loop ([s 0] [b 0] [acc null])
      (cond [(>= s (vector-length str)) acc]
            [else
             (define l (char-utf-8-length (vector-ref str s)))
             (define adds (build-list l (const (- b s))))
             (loop (add1 s) (+ b l) (append adds acc))])))
  (define mapping (list->vector (reverse lmapping)))
  (lambda (offset)
    (if (> offset (vector-length mapping))
        (vector-ref mapping (sub1 (vector-length mapping)))
        (vector-ref mapping (sub1 offset)))))

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
