#lang racket/base
(require "cover.rkt" "format.rkt" "private/contracts.rkt" "private/format-utils.rkt"
         "private/raw.rkt" racket/contract)
(provide
 (contract-out
  [coverage/c contract?]
  [file-coverage/c contract?]
  [test-files! (->* () (#:submod symbol?) #:rest (listof path-string?) any/c)]
  [clear-coverage! (-> any)]
  [get-test-coverage (-> coverage/c)]
  [make-covered?
   (-> file-coverage/c path-string?
       (->* (exact-positive-integer?)
            (#:byte? boolean?)
            (or/c 'yes 'no 'missing)))]
  [generate-coveralls-coverage coverage-gen/c]
  [generate-html-coverage coverage-gen/c]
  [generate-raw-coverage coverage-gen/c]))
