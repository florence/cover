#lang setup/infotab
(define name "better-test")
(define deps '("base" "errortrace-lib" "rackunit-lib"
               "syntax-color-lib"))

(define raco-commands
  '(("better-test" (submod better-test/raco main) "a better testing library" 100)))
