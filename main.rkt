#lang racket/base
(require "cover.rkt" "format.rkt" "private/contracts.rkt" "private/format-utils.rkt"
         racket/contract)
(provide
 (contract-out
  [test-files! (->* () () #:rest path-string? any/c)]
  [clear-coverage! (-> any)]
  [get-test-coverage (-> coverage/c)]
  [make-covered?
   (-> file-coverage/c path-string?
       (->* (exact-positive-integer?)
            (#:byte? boolean?)
            (or/c 'yes 'no 'missing)))]
  [generate-coveralls-coverage (->* (coverage/c) (path-string?) any)]
  [generate-html-coverage (->* (coverage/c) (path-string?) any)]))
