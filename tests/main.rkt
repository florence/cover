#lang racket
(require better-test racket/runtime-path rackunit)

(define (test-dir d)
  (define program (string-append d "/prog.rkt"))
  (define covered (string-append d "/coverage.rktl"))

  (test-files! program)

  (define actual-coverage (hash-ref (get-test-coverage) program))
  (define expected-coverage (ranges->numbers (with-input-from-file covered read)))

  (test-begin
   (for ([i expected-coverage])
     (check-true (covered? i actual-coverage)
                 (format "expected char ~a to be covered, but it was not, in: ~s"
                         i d))))

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
             [b (in-value (first map))]
             [srcloc (in-value (second map))]
             #:when (within? i srcloc))
    b))

(define (within? i src)
  (match src
    [(srcloc _ _ _ start range)
     (>= start i (+ start range))]))

(module+ test
  (define-runtime-path-list test-dirs '("basic"))
  (for-each (compose test-dir path->string) test-dirs))
