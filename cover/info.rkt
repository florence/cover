#lang setup/infotab

(define cover-formats '(("html" cover generate-html-coverage)
                        ("raw" cover generate-raw-coverage)))

(define test-omit-paths (list "tests/error-file.rkt" "scribblings"))
(define cover-omit-paths (list "tests/nested.rkt" "tests/bfs+module.rkt"))

(define test-command-line-arguments '(("tests/arg.rkt" ("a"))))

(define raco-commands
  '(("cover" (submod cover/raco main) "a code coverage tool" 30)))

(define scribblings '(("scribblings/cover.scrbl" (multi-page))))
