#lang racket/base
(provide ->relative ->absolute)
(require racket/list)
(module+ test (require rackunit))

;; PathString -> Path
(define (->relative path)
  (if (relative-path? path)
      (build-path path)
      (let-values ([(_ lst)
                    (split-at (explode-path path)
                              (length (explode-path (current-directory))))])
        (apply build-path lst))))

(module+ test
  (parameterize ([current-directory (build-path "/test")])
    (check-equal? (->relative "a")
                  (build-path "a"))
    (check-equal? (->relative "/test/a/b")
                  (build-path "a" "b"))))

(define (->absolute path)
  (if (absolute-path? path)
      (if (string? path) path (path->string path))
      (path->string (simplify-path (build-path (current-directory) path)))))
(module+ test
  (parameterize ([current-directory (build-path "/")])
    (check-equal? (->absolute "a") "/a")
    (check-equal? (->absolute "/a") "/a")
    (check-equal? (->absolute (build-path "a")) "/a")
    (check-equal? (->absolute (build-path "/a")) "/a")))
