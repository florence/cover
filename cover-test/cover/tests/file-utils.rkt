#lang racket

(require racket/list racket/path)
(require rackunit cover/private/file-utils)

(parameterize ([current-directory (build-path "/test")])
  (check-equal? (->relative "a")
                (build-path "a"))
  (check-equal? (->relative "/test/a/b")
                (build-path "a" "b")))

(parameterize ([current-directory (build-path "/")])
  (check-equal? (->absolute "a") "/a")
  (check-equal? (->absolute "/a") "/a")
  (check-equal? (->absolute (build-path "a")) "/a")
  (check-equal? (->absolute (build-path "/a")) "/a"))