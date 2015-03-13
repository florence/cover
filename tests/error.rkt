#lang racket
(require "../main.rkt" rackunit racket/runtime-path)

(define-runtime-path error "error-file.rkt")
(define-runtime-path main "main.rkt")
(test-begin
 (after
  (define (do-test files)
    (define o (open-output-string))
    (parameterize ([current-error-port o])
      (apply test-files! files))
    (define s (get-output-string o))
    (define c (get-test-coverage))
    (define covered (hash-keys c))
    (for-each
     (lambda (x) (check-not-false (member x covered) s))
     files)
    (clear-coverage!))
  (define files (map path->string (list error main)))
  (do-test files)
  (do-test (reverse files))
  (clear-coverage!)))
