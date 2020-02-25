#lang racket
(require racket/runtime-path cover rackunit cover/private/shared)
(define-runtime-path reader.rkt "reader.rkt")
(check-not-exn
 (thunk
  (define r (path->string reader.rkt))
  (test-files! r)
  (define c (curry (get-test-coverage) r))
  (c 10)))
