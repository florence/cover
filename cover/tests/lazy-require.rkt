#lang racket
(begin-for-syntax
  (require racket/lazy-require)
  (lazy-require [racket/syntax (format-symbol)])
  (void (format-symbol "a~a" 'a)))
