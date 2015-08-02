#lang racket/base
(require "cover.rkt" "format.rkt" "private/contracts.rkt" "private/format-utils.rkt"
         "private/raw.rkt" racket/contract)

(define (not-impersonated/c c)
  (and/c (lambda (v) (not (impersonator? v)))
         c))

(provide
 (contract-out
  [coverage/c contract?]

  [test-files! (->* () (#:submod symbol?
                        #:env environment?)
                    #:rest
                    (listof (or/c path-string?
                                  (list/c path-string?
                                          (not-impersonated/c
                                           (vectorof (not-impersonated/c string?) #:immutable #t)))))
                    any)]

  [environment? (-> any/c any/c)]
  [environment-namespace (-> environment? namespace?)]
  [environment-compile
   (-> environment? (any/c boolean? . -> . compiled-expression?))]

  [clear-coverage! (-> any)]
  [make-cover-environment (->* () ((-> namespace?)) environment?)]
  [current-cover-environment (parameter/c environment?)]

  [get-test-coverage (->* () (environment?) coverage/c)]

  [irrelevant-submodules (parameter/c (or/c #f (listof symbol?)))]

  [generate-html-coverage coverage-gen/c]
  [generate-raw-coverage coverage-gen/c]))
