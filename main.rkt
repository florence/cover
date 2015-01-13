#lang racket/base
(require "cover.rkt" "format.rkt" "private/contracts.rkt" "private/format-utils.rkt"
         "private/raw.rkt" racket/contract)
(provide
 (contract-out
  [coverage/c contract?]
  [file-coverage/c contract?]
  [test-files! (->* () (#:submod symbol?)
                    #:rest (lisdtof path-string?)
                    ;; TODO when we figure out the contract issue we will change this
                    #;
                    (listof (or/c path-string?
                                  (list/c path-string? (vectorof string?))))
                    any)]
  [clear-coverage! (-> any)]
  [get-test-coverage (-> coverage/c)]
  [make-covered?
   (-> file-coverage/c path-string?
       (->* (exact-positive-integer?)
            (#:byte? boolean?)
            (or/c 'covered 'uncovered 'irrelevant)))]
  [generate-coveralls-coverage coverage-gen/c]
  [generate-html-coverage coverage-gen/c]
  [generate-raw-coverage coverage-gen/c]))
