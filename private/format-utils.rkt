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
         "shared.rkt")

(module+ test (require rackunit "../cover.rkt" racket/runtime-path racket/set))

;;;;; a Coverage is the output of (get-test-coverage)
;;;;; a FileCoverage is the values of the hashmap from (get-test-coverage)

;;;;; utils

;;; a Cover is (U 'covered 'uncovered 'irrelevant)

;; [Hashof PathString [Hashof Natural Cover]]

;; A Covered? is a [Nat [#:byte? Boolean] -> Cover]

;; FileCoverage PathString #:ignored-submods (maybe (listof symbol)) -> Covered?
(define (make-covered? c path)
  (define submods (irrelevant-submodules))
  (define vec
    (list->vector (string->list (file->string path))))
  (define file-location-coverage-cache
    (coverage-cache-file path c submods))
  (lambda (loc)
    (hash-ref file-location-coverage-cache loc
              'missing)))

;; (or/c #f (listof symbol))
(define irrelevant-submodules (make-parameter #f))

;; Path FileCoverage -> [Hashof Natural Cover]
;; build a hash caching coverage info for that file
(define (coverage-cache-file f c submods)
  (vprintf "caching coverage info for ~s\n" f)
  (with-input-from-file f
    (thunk
     (define lexer
       (maybe-wrap-lexer
        (with-handlers ([exn:fail:read? (const racket-lexer)])
          (define f (read-language))
          (if f
              (f 'color-lexer racket-lexer)
              racket-lexer))))
     (define irrelevant? (make-irrelevant? lexer f submods))
     (define file-length (string-length (file->string f)))
     (define cache
       (for/hash ([i (in-range 1 (add1 file-length))])
         (values i
                 (cond [(irrelevant? i) 'irrelevant]
                       [else (raw-covered? i c)]))))
     cache)))

(define (maybe-wrap-lexer lexer)
  (if (procedure-arity-includes? lexer 3)
      lexer
      (λ (in offset mode)
        (define-values (a b c d e) (lexer in))
        (values a b c d e 0 #f))))

;; Lexer(in the sence of color:text<%>) InputPort (Maybe (Listof Symbol)) -> (Natural -> Boolean)
;; builds a function that determines if a given location in that port is irrelivent.
(define (make-irrelevant? lexer f submods)
  (define s (mutable-set))
  (define-values (for-lex for-str) (replicate-file-port f (current-input-port)))
  (define str (apply vector (string->list (port->string for-str))))
  (define init-offset (- (string-length (file->string f))
                         (vector-length str)))

  (define offset (make-byte->str-offset str))

  (let loop ([mode #f])
    (define-values (v type _m start end backup-dist new-mode/ds)
      (lexer for-lex 0 mode))
    (define new-mode (if (dont-stop? new-mode/ds)
                         (dont-stop-val new-mode/ds)
                         new-mode/ds))
    (case type
      [(eof) (void)]
      [(comment sexp-comment no-color white-space)
       (for ([i (in-range (- start (offset start)) (- end (offset end)))])
         (set-add! s (+ init-offset i)))
       (loop new-mode)]
      [else (loop new-mode)]))
  (define stx
    (with-input-from-file f
      (thunk (with-module-reading-parameterization read-syntax))))

  (define offset/mod (make-byte->str-offset str))
  (let loop ([stx stx] [first? #t])
    (define (loop* stx) (loop stx #f))
    (syntax-parse stx
      #:datum-literals (module module* module+ begin-for-syntax)
      [((~or module module* module+ begin-for-syntax)
        n:id
        e ...)
       #:when (and (not first?)
                   (submods
                    . implies .
                    (member (syntax-e #'n) submods)))
       (define ?start (syntax-position stx))
       (when ?start
         (define start (- ?start (* 2 (offset/mod ?start))))
         (define end (+ start (syntax-span stx)))
         (for ([i (in-range start end)])
           (set-add! s i)))]
      [(e ...) (for-each loop* (syntax->list #'(e ...)))]
      [_else (void)]))
  (lambda (i) (set-member? s i)))

;; Path FilePort -> FilePort FilePort
;; creates two ports to that file at the same position at the first
(define (replicate-file-port f p)
  (define f1 (open-input-file f))
  (define f2 (open-input-file f))
  (file-position f1 (file-position p))
  (file-position f2 (file-position p))
  (values f1 f2))

;; Natural Coverage -> (U 'covered 'uncovered 'irrelevant)
;; lookup i in c. irrelevant if its not contained
(define (raw-covered? i c)
  (define loc i)
  (define-values (mode _)
    (for/fold ([mode 'none] [last-start 0])
              ([pair (in-list c)])
      (match pair
        [(list m (srcloc _ _ _ start range))
         (if (and (<= start loc (+ start range -1))
                  (or (eq? mode 'none)
                      (> start last-start)))
             (values m start)
             (values mode last-start))])))
  (case mode
    [(#t) 'covered]
    [(#f) 'uncovered]
    [else 'irrelevant]))

;; String -> (Natural -> Natural)
;; used for determining character/byte offsets for a given
;; 1 indexed byte locaiton
(define ((make-byte->str-offset str) offset)
  (let loop ([s 0] [b 0])
    (cond [(or (= (sub1 offset) b)
               (>= s (vector-length str)))
           (- b s)]
          [else
           (define l (char-utf-8-length (vector-ref str s)))
           (loop (add1 s) (+ b l))])))

(module+ test
  (define-runtime-path path2 "../tests/prog.rkt")
  (parameterize ([irrelevant-submodules #f])
    (test-begin
     (parameterize ([current-cover-environment (make-cover-environment)])
       (define f (path->string (simplify-path path2)))
       (test-files! f)
       (define coverage (hash-ref (get-test-coverage) f))
       (define covered? (make-covered? coverage f))
       (check-equal? (covered? 14) 'irrelevant)
       (check-equal? (covered? 17) 'irrelevant)
       (check-equal? (covered? 28) 'irrelevant)
       (check-equal? (covered? 35) 'covered)
       (check-equal? (covered? 52) 'irrelevant)
       (check-equal? (covered? 53) 'irrelevant)
       (check-equal? (covered? 54) 'irrelevant)))))
