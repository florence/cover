#lang racket
(require cover rackunit racket/runtime-path (only-in "../cover.rkt" coverage-wrapper-map))
(define-runtime-path file "cross-phase-persist.rkt")
(parameterize ([current-cover-environment (make-cover-environment)])
  (test-files! file)
  (check-equal? (coverage-wrapper-map (get-test-coverage)) (hash)))
