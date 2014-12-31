#lang racket
(provide get-percentages/top get-percentages/file covered?)
(require syntax/modread syntax/parse unstable/sequence syntax-color/racket-lexer)
(module+ test (require rackunit "../cover.rkt" racket/runtime-path))

;;;;; a Coverage is the output of (get-test-coverage)
;;;;; a FileCoverage is the values of the hashmap from (get-test-coverage)

;;;;; percentage
;; A Percentage is a [HashMap Type Real∈[0,1]]
;; a Type is one of: (update this as needed)
;; 'expr

;;  TODO this needs not count submodules and test directories

;; Coverage -> Percentage
(define (get-percentages/top coverage)
  (hash
   'expr (file-percentages->top expr-percentage coverage)))

(define (file-percentages->top get-% coverage)
  (define per-file
    (for/list ([(f v) coverage])
      (call-with-values (thunk (get-% f v)) list)))
  (define total (for/sum ([v per-file]) (second v)))
  (for/sum ([v per-file])
    (* (first v) (/ (second v) total))))

;; PathString FileCoverage -> Percentage
(define (get-percentages/file path coverage)
  (hash
   'expr (first (call-with-values (thunk (expr-percentage path coverage)) list))))

;;; percentage generators. each one has the type:
;; FileCoverage -> Real∈[0,1] Natural
;; there the Real is the percentage covered
;; and the Natural is the number of things of that type in the file

(define (expr-percentage path coverage)
  (define (is-covered? e)
    ;; we don't need to look at the span because the coverage is expression based
    (define p (syntax-position e))
    (covered? p coverage path))

  (define e
    (with-module-reading-parameterization
        (thunk (with-input-from-file path read-syntax))))
  (define (ret e)
    (values (e->n e) (a->n e)))
  (define (a->n e)
    (case (is-covered? e)
      [(yes no) 1]
      [else 0]))
  (define (e->n e)
    (if (eq? (is-covered? e) 'yes) 1 0))
  (define-values (covered count)
    (let recur ([e e])
      (syntax-parse e
        [(v ...)
         (for/fold ([covered (e->n e)] [count (a->n e)])
                   ([e (in-syntax e)])
           (define-values (cov cnt) (recur e))
           (values (+ covered cov)
                   (+ count cnt)))]
        [e:expr (ret #'e)]
        [_ (values 0 0)])))
  (values (/ covered count) count))

(module+ test
  (define-runtime-path path "../tests/basic/prog.rkt")
  (test-begin
   (define f (path->string (simplify-path path)))
   (test-files! f)
   (define-values (result _) (expr-percentage f (hash-ref (get-test-coverage) f)))
   (check-equal? result 1)
   (clear-coverage!)))

;;;;; utils

;;; a Cover is (U 'yes 'no 'missing)

;; [Hashof PathString [Hashof Natural Cover]]
(define file-location-coverage-cache (make-hash))

;; Natural FileCoverage PathString -> Cover
(define (covered? loc c path)
  (define file-cache
    (let ([v (hash-ref file-location-coverage-cache path #f)])
      (if v v (coverage-cache-file! path c))))
  (hash-ref file-cache loc))


;; Path FileCoverage -> [Hashof Natural Cover]
(define (coverage-cache-file! f c)
  (with-input-from-file f
    (thunk
     (define lexer
       ((read-language) 'color-lexer racket-lexer))
     (define irrelevant? (make-irrelevant? lexer f))
     (define file-length (string-length (file->string f)))
     (define cache
       (for/hash ([i (range 1 (add1 file-length))])
         (values i
                 (cond [(irrelevant? i) 'missing]
                       [else (raw-covered? i c)]))))
     (hash-set! file-location-coverage-cache
                f
                cache)
     cache)))

;; TODO should we only ignore test (and main) submodules?
(define (make-irrelevant? lexer f)
  (define s (mutable-set))
  (let loop ()
    (define-values (_v type _m start end) (lexer (current-input-port)))
    (case type
      [(eof) (void)]
      [(comment sexp-comment no-color)
       (for ([i (in-range start end)])
         (set-add! s i))
       (loop)]
      [else (loop)]))
  (define stx
    (with-input-from-file f
      (thunk (with-module-reading-parameterization read-syntax))))
  (let loop ([stx stx] [first? #t])
    (define (loop* stx) (loop stx #f))
    (syntax-parse stx
      #:datum-literals (module module* module+)
      [((~or module module* module+) e ...)
       #:when (not first?)
       (define pos (syntax-position stx))
       (when pos
         (for ([i (in-range pos (+ pos (syntax-span stx)))])
           (set-add! s i)))]
      [(e ...) (for-each loop* (syntax->list #'(e ...)))]
      [_else (void)]))
  (lambda (i) (set-member? s i)))

(define (in-syntax-object? i stx)
  (define p (syntax-position stx))
  (define r (syntax-span stx))
  (<= p i (+ p r)))

(define (raw-covered? loc c)
  (define-values (mode _)
    (for/fold ([mode 'none] [last-start 0])
              ([pair c])
      (match pair
        [(list m (srcloc _ _ _ start range))
         (if (and (<= start loc (+ start range -1))
                  (or (eq? mode 'none)
                      (> start last-start)))
             (values m start)
             (values mode last-start))])))
  (case mode
    [(#t) 'yes]
    [(#f) 'no]
    [else 'missing]))

(module+ test
  (define-runtime-path path2 "../tests/prog.rkt")
  (test-begin
   (define f (path->string (simplify-path path2)))
   (test-files! f)
   (define coverage (hash-ref (get-test-coverage) f))
   (check-equal? (covered? 17 coverage f) 'missing)
   (check-equal? (covered? 35 coverage f) 'yes)
   (clear-coverage!)))
