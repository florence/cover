#lang racket/base
(provide verbose vprintf
         logger-init-message
         logger-covered-message)
(define verbose (make-parameter #f))

(define logger-init-message "init")
(define logger-covered-message "covered")

;; like printf but only in verbose mode
(define o (current-output-port))
(define (vprintf #:printer [printer printf] . a)
  (when (verbose)
    (parameterize ([current-output-port o])
      (apply printer a))))
