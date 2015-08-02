#lang racket
(require racket/runtime-path "../main.rkt" rackunit "../private/shared.rkt")
(define-runtime-path reader.rkt "reader.rkt")
(parameterize ([verbose #t])
  (check-not-exn
   (thunk
    (define r (path->string reader.rkt))
    (test-files! r)
    (define c (curry (get-test-coverage) r))
    (c 10))))
