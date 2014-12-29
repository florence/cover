#lang setup/infotab
(define name "cover")
(define deps '("base" "errortrace-lib" "rackunit-lib"
               "syntax-color-lib"))

(define raco-commands
  '(("cover" (submod cover/raco main) "a code coverage tool" 100)))
