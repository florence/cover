#lang info
(define collection 'multi)
(define pkg-desc "benckmarks for cover")

(define deps
  '("draw-lib"
    "plot-lib"
    ["cover-lib" #:version "3.3.3"]
    "base"
    "custom-load"
    #;"typed-racket-lib"
    #;"typed-racket-test"
    #;"typed-racket-more"
    "math-lib"
    "pict-lib"
    "pict-test"
    "racket-benchmarks"))
