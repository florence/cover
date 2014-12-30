#lang racket/base
(require racket/pretty)
(provide generate-raw-coverage)
(define (generate-raw-coverage coverage [dir "coverage"])
  (with-output-to-file (build-path dir "coverage.rktl")
    (lambda () (pretty-write coverage))))
