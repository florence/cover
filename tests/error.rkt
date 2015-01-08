#lang racket
(require "../main.rkt" rackunit racket/runtime-path)

(define-runtime-path error "error-file.rkt")
(define-runtime-path main "main.rkt")
(test-begin
 (after
  (define (do-test files)
    (apply test-files! files)
    (define c (get-test-coverage))
    (define covered (hash-keys c))
    (for-each
     (lambda (x) (check-not-false (member x covered)))
     files)
    (clear-coverage!))
  (define files (map path->string (list error main)))
  (do-test files)
  (do-test (reverse files))
  (clear-coverage!)))
