#lang info

(define name "cover")
(define collection 'multi)

(define version "3.0.3")

(define deps '(("base" #:version "6.1.1") "errortrace-lib" "rackunit-lib"
               "syntax-color-lib" "compiler-lib" "custom-load" "data-lib"))

(define build-deps
  '("racket-doc" "scribble-lib" "typed-racket-doc" "htdp-lib"
    "net-doc" "scribble-doc" "at-exp-lib" "scheme-lib" "typed-racket-lib"
    "macro-debugger"))

(define pkg-authors '("spencer@florence.io"))
