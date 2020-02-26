#lang racket/base
(provide ->relative ->absolute)
(require racket/list racket/path)
(define (->relative path)
  (build-path
   (find-relative-path
    (simple-form-path (current-directory))
    (simple-form-path path))))
(define (->absolute path)
  (if (absolute-path? path)
      (path->string (simple-form-path path))
      (path->string (simple-form-path (build-path (current-directory) path)))))