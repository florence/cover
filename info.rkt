#lang setup/infotab
(define name "cover")
(define deps '("base" "errortrace-lib" "rackunit-lib"
               "syntax-color-lib"))
(define build-deps
  '("racket-doc" "scribble-lib"))

(define raco-commands
  '(("cover" (submod cover/raco main) "a code coverage tool" 30)))

(define scribblings '(("scribblings/main.scrbl" ())))
