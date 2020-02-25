#lang info

(define collection 'multi)
(define version "3.3.0")
(define pkg-desc "A code coverage library -- implementation")

(define deps '("base"
               "compiler-lib"
               "custom-load"
               "data-lib"
               "errortrace-lib"
               "syntax-color-lib"
               "testing-util-lib"))
(define build-deps '("cover-test"
                     "rackunit-lib"))
