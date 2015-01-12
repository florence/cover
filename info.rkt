#lang setup/infotab
(define name "cover")
(define collection "cover")
(define deps '("base" "errortrace-lib" "rackunit-lib"
               "syntax-color-lib"))
(define build-deps
  '("racket-doc" "scribble-lib" "typed-racket-doc"))

(define raco-commands
  '(("cover" (submod cover/raco main) "a code coverage tool" 30)))

(define scribblings '(("scribblings/cover.scrbl" (multi-page))))

(define test-omit-paths (list "tests/error-file.rkt"))

(define cover-formats '(("html" cover generate-html-coverage)
                        ("coveralls" cover generate-coveralls-coverage)
                        ("raw" cover generate-raw-coverage)))

(define version "1.0.0")
