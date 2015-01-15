#lang racket/base
(provide make-covered?)
(require racket/file
         racket/function
         racket/list
         racket/match
         racket/port
         racket/set
         syntax-color/racket-lexer
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

;; FileCoverage PathString -> Covered?
(define (make-covered? c path)
  (define vec
    (list->vector (string->list (file->string path))))
  (define file/byte->str-offset (make-byte->str-offset vec))
  (define file-location-coverage-cache
    (coverage-cache-file path c))
  (lambda (loc #:byte? [byte? #f])
    (hash-ref file-location-coverage-cache (if (not byte?) loc (- loc (file/byte->str-offset loc)))
              'missing)))


;; Path FileCoverage OffsetFunc -> [Hashof Natural Cover]
(define (coverage-cache-file f c)
  (vprintf "caching coverage info for ~s\n" f)
  (with-input-from-file f
    (thunk
     (define lexer
       (with-handlers ([exn:fail:read? (const racket-lexer)])
         (define f (read-language))
         (if f
             (f 'color-lexer racket-lexer)
             racket-lexer)))
     (define irrelevant? (make-irrelevant? lexer f))
     (define file-length (string-length (file->string f)))
     (define cache
       (for/hash ([i (in-range 1 (add1 file-length))])
         (values i
                 (cond [(irrelevant? i) 'irrelevant]
                       [else (raw-covered? i c)]))))
     cache)))

;; TODO should we only ignore test (and main) submodules?
(define (make-irrelevant? lexer f)
  (define s (mutable-set))
  (define-values (for-lex for-str) (replicate-file-port f (current-input-port)))
  (define str (apply vector (string->list (port->string for-str))))
  (define init-offset (- (string-length (file->string f))
                         (vector-length str)))

  (define offset (make-byte->str-offset str))

  (let loop ()
    (define-values (v type _m start end) (lexer for-lex))
    (case type
      [(eof) (void)]
      [(comment sexp-comment no-color white-space)
       (for ([i (in-range (- start (offset start)) (- end (offset end)))])
         (set-add! s (+ init-offset i)))
       (loop)]
      [else (loop)]))
  (define stx
    (with-input-from-file f
      (thunk (with-module-reading-parameterization read-syntax))))

  (define offset/mod (make-byte->str-offset str))
  (let loop ([stx stx] [first? #t])
    (define (loop* stx) (loop stx #f))
    (syntax-parse stx
      #:datum-literals (module module* module+ begin-for-syntax)
      [((~or module module* module+ begin-for-syntax)
        e ...)
       #:when (not first?)
       (define ?start (syntax-position stx))
       (when ?start
         (define start (- ?start (* 2 (offset/mod ?start))))
         (define end (+ start (syntax-span stx)))
         (for ([i (in-range start end)])
           (set-add! s i)))]
      [(e ...) (for-each loop* (syntax->list #'(e ...)))]
      [_else (void)]))
  (lambda (i) (set-member? s i)))

(define (replicate-file-port f p)
  (define f1 (open-input-file f))
  (define f2 (open-input-file f))
  (file-position f1 (file-position p))
  (file-position f2 (file-position p))
  (values f1 f2))


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
  (test-begin
   (define f (path->string (simplify-path path2)))
   (test-files! f)
   (define coverage (hash-ref (get-test-coverage) f))
   (define covered? (make-covered? coverage f))
   (check-equal? (covered? 14) 'irrelevant)
   (check-equal? (covered? 14 #:byte? #t) 'irrelevant)
   (check-equal? (covered? 17) 'irrelevant)
   (check-equal? (covered? 28) 'irrelevant)
   (check-equal? (covered? 35) 'covered)
   (check-equal? (covered? 50) 'uncovered)
   (check-equal? (covered? 51 #:byte? #t) 'uncovered)
   (check-equal? (covered? 52) 'irrelevant)
   (check-equal? (covered? 53) 'irrelevant)
   (check-equal? (covered? 54) 'irrelevant)
   (clear-coverage!)))
