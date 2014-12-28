#lang racket
(require better-test racket/runtime-path rackunit)

(define (test-dir d)
  (define files
    (for*/list ([p (directory-list d)]
                [s (in-value (path->string (build-path d p)))]
                #:when (regexp-match #rx"\\.rkt$" s))
      s))
  (define covered
    (for/hash ([f files])
      (values f
              (path->string (path-replace-suffix f ".rktl")))))

  (apply test-files! files)

  (define coverage (get-test-coverage))
  (for ([(program cover) covered])
    (define actual-coverage (hash-ref coverage program))
    (define expected-coverage (ranges->numbers (with-input-from-file cover read)))
    (test-begin
     (for ([i expected-coverage])
       (check-true (covered? i actual-coverage)
                   (format "expected char ~a to be covered, but it was not, in: ~s"
                           i program)))))

  (clear-coverage!))

(define (ranges->numbers range)
  (match range
    [(list) null]
    [(cons (list a b) r)
     (if (equal? a b)
         (ranges->numbers r)
         (cons a (ranges->numbers (cons (list (add1 a) b) r))))]))

(define (covered? i map)
  (for*/and ([l map]
             [b (in-value (first l))]
             [srcloc (in-value (second l))]
             #:when (within? i srcloc))
    b))

(define (within? i src)
  (match src
    [(srcloc _ _ _ start range)
     (>= start i (+ start range))]))

(module+ test
  (define-runtime-path-list test-dirs '("basic"))
  (for-each (compose test-dir path->string) test-dirs))
