#lang racket
(require cover rackunit racket/runtime-path)
(define-runtime-path file "cross-phase-persist.rkt")
(parameterize ([current-cover-environment (make-cover-environment)])
  (test-files! file)
  (check-equal? (get-test-coverage) (hash)))
