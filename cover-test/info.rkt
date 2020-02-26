#lang info

(define collection 'multi)
(define deps '("base"
               "cover-lib"))
(define pkg-desc "A code coverage library -- tests")
(define pkg-authors '("spencerflorence@gmail.com"))

(define build-deps '("data-lib"
                     "syntax-color-lib"
                     "compiler-lib"
                     "custom-load"
                     "at-exp-lib"
                     "base"
                     "htdp-lib"
                     "macro-debugger"
                     "rackunit-lib"
                     "typed-racket-lib"))
