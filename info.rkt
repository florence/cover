#lang setup/infotab
(define name "better-test")
(define build-deps '("rackunit-lib"))
(define deps '("base" "errortrace-lib"))

(define raco-commands
  '(("better-test" (submod better-test/raco main) "a better testing library" 100)))
