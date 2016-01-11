#lang racket/base
(require racket/pretty racket/file "../cover.rkt")
(provide generate-raw-coverage)
(define (generate-raw-coverage coverage files [dir "coverage"])
  (make-directory* dir)
  (with-output-to-file (build-path dir "coverage.rktl")
    #:exists 'replace
    (lambda () (pretty-write (coverage-wrapper-map coverage)))))
