#lang info
(define collection 'multi)
(define pkg-desc "benckmarks for cover")

(define deps
  '(["cover-lib" #:version "3.3.3"]
    ["base" #:version "7.6"]
    "custom-load"
    "typed-racket-lib"
    "typed-racket-test"
    "typed-racket-more"
    "math-lib"
    "pict-lib"
    "pict-test"
    "racket-benchmarks"))
