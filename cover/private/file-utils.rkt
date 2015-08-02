#lang racket/base
(provide ->relative ->absolute)
(require racket/list racket/path)
(module+ test (require rackunit))

;; PathString -> Path
(define (->relative path)
  (build-path
   (find-relative-path
    (simple-form-path (current-directory))
    (simple-form-path path))))

(module+ test
  (parameterize ([current-directory (build-path "/test")])
    (check-equal? (->relative "a")
                  (build-path "a"))
    (check-equal? (->relative "/test/a/b")
                  (build-path "a" "b"))))

(define (->absolute path)
  (if (absolute-path? path)
      (path->string (simple-form-path path))
      (path->string (simple-form-path (build-path (current-directory) path)))))
(module+ test
  (parameterize ([current-directory (build-path "/")])
    (check-equal? (->absolute "a") "/a")
    (check-equal? (->absolute "/a") "/a")
    (check-equal? (->absolute (build-path "a")) "/a")
    (check-equal? (->absolute (build-path "/a")) "/a")))
