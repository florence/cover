#lang info
(define test-omit-paths (list "tests/error-file.rkt" "scribblings" "tests/basic/raise.rkt"))
(define cover-omit-paths (list "tests/nested.rkt" "tests/bfs+module.rkt"))

(define test-command-line-arguments '(("tests/arg.rkt" ("a"))))
