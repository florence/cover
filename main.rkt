#lang racket/base
(require "cover.rkt" "format.rkt" "private/contracts.rkt" "private/format-utils.rkt"
         racket/contract)
(provide
 (contract-out
  [test-files! (->* () () #:rest path-string? any/c)]
  [clear-coverage! (-> any)]
  [get-test-coverage (-> coverage/c)]
  [covered? (-> exact-positive-integer? file-coverage/c path-string? (or/c 'yes 'no 'missing))]
  [generate-coveralls-coverage (->* (coverage/c) (path-string?) any)]
  [generate-html-coverage (->* (coverage/c) (path-string?) any)]))
