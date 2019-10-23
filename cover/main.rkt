#lang racket/base
(require "cover.rkt" "private/contracts.rkt"
         racket/contract)

(define (not-impersonated/c c)
  (and/c (lambda (v) (not (impersonator? v)))
         c))

(provide
 (contract-out
  [coverage/c contract?]

  [test-files! (->* () (#:submod (or/c symbol? (listof symbol?))
                        #:env environment?
                        #:dont-compile (listof path-string?))
                    #:rest
                    (listof (or/c path-string?
                                  (list/c path-string?
                                          (not-impersonated/c
                                           (vectorof (not-impersonated/c string?) #:immutable #t)))))
                    any)]

  [environment? (-> any/c any/c)]

  [make-cover-environment (->* () (namespace?) environment?)]
  [current-cover-environment (parameter/c environment?)]

  [get-test-coverage (->* () (environment?) coverage/c)]

  [irrelevant-submodules (parameter/c (or/c #f (listof symbol?)))]

  
  ))
