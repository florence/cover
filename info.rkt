#lang setup/infotab
(define name "cover")
(define collection "cover")
(define deps '(("base" #:version "6.1.1") "errortrace-lib" "rackunit-lib"
               "syntax-color-lib" "compiler-lib"))
(define build-deps
  '("racket-doc" "scribble-lib" "typed-racket-doc" "htdp-lib"))

(define raco-commands
  '(("cover" (submod cover/raco main) "a code coverage tool" 30)))

(define scribblings '(("scribblings/cover.scrbl" (multi-page))))

(define test-omit-paths (list "tests/error-file.rkt" "scribblings"))

(define cover-formats '(("html" cover generate-html-coverage)
                        ("coveralls" cover generate-coveralls-coverage)
                        ("raw" cover generate-raw-coverage)))

(define test-command-line-arguments '(("tests/arg.rkt" ("a"))))

(define version "1.1.0")
