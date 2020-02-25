#lang info

(define cover-formats
  '(("html" cover generate-html-coverage)
    ;; Undocumented. Meant for internal/debugging only
    ("raw" cover generate-raw-coverage)))

(define raco-commands
  '(("cover" (submod cover/raco main) "a code coverage tool" 30)))
