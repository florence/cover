#lang racket/base
(require "cover.rkt" "format.rkt" "private/contracts.rkt" "private/format-utils.rkt"
         "private/raw.rkt" racket/contract)

(define (not-impersonated/c c)
  (and/c (lambda (v) (not (impersonator? v)))
         c))

(provide
 (contract-out
  [coverage/c contract?]

  [file-coverage/c contract?]
  [test-files! (->* () (#:submod symbol?
                        #:env environment?)
                    #:rest
                    (listof (or/c (or/c path-string? input-port?)
                                  (list/c (or/c path-string? input-port?)
                                          (not-impersonated/c
                                           (vectorof (not-impersonated/c string?) #:immutable #t)))))
                    any)]

  [environment? (-> any/c any/c)]
  [environment-namespace (-> environment? namespace?)]
  [environment-compile
   (-> environment? (any/c boolean? . -> . compiled-expression?))]

  [clear-coverage! (-> any)]
  [make-clean-cover-environment (->* () ((-> namespace?)) environment?)]
  [current-cover-environment (parameter/c environment?)]

  [get-test-coverage (->* () (environment?) coverage/c)]

  [irrelevant-submodules (parameter/c (or/c #f (listof symbol?)))]
  [make-covered?
   (-> file-coverage/c path-string?
       (->* (exact-positive-integer?)
            (#:byte? boolean?)
            (or/c 'covered 'uncovered 'irrelevant)))]

  [generate-coveralls-coverage coverage-gen/c]
  [generate-html-coverage coverage-gen/c]
  [generate-raw-coverage coverage-gen/c]))
