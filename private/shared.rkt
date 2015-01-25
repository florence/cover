#lang racket/base
(provide verbose vprintf)
(require (for-syntax racket/base))
(define verbose (make-parameter #f))

;; like printf but only in verbose mode
(define o (current-output-port))
(define (vprintf #:printer [printer printf] . a)
  (when (verbose)
    (parameterize ([current-output-port o])
      (apply printer a))))
