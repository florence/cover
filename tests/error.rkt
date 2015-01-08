#lang racket
(require "../main.rkt" rackunit)

(test-begin
 (after
  (define (do-test files)
    (apply test-files! files)
    (define c (get-test-coverage))
    (define covered
      (map (compose path->string last explode-path) files))
    (for-each
     (lambda (x) (check-not-false (member x covered)))
     files)
    (clear-coverage!))
  (define files (list "error-file.rkt" "prog.rkt"))
  (do-test files)
  (do-test (reverse files))
  (clear-coverage!)))
