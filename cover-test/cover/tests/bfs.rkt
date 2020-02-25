#lang racket
;; this is a comment
(+ 1 2)
(λ (x) 3)
(module+ test 20)
(λ (x) 3)
(module+ test 20)
(λ (x) 3)
(module+ test 20)

(begin-for-syntax
  (require (for-syntax (only-in racket/base require for-syntax #%app begin-for-syntax #%datum only-in)))
  ;; this is a comment
  (+ 1 2)
  (λ (x) 3)
  (module+ test1 20)
  (λ (x) 3)
  (module+ test1 20)
  (λ (x) 3)
  (module+ test1 20)
  (begin-for-syntax
    (require (only-in racket/base + λ module*))
    (require (for-syntax (only-in racket/base require for-syntax #%app begin-for-syntax #%datum only-in)))
    (+ 1 2)
    (λ (x) 3)
    (λ (x) 3)
    (λ (x) 3)
    (module* test2 racket 20)

    (begin-for-syntax
      (require (only-in racket/base + λ module*))
      (require (for-syntax (only-in racket/base require for-syntax #%app begin-for-syntax #%datum only-in)))
      (+ 1 2)
      (λ (x) 3)
      (λ (x) 3)
      (λ (x) 3)
      (module* test3 racket 20)

      (begin-for-syntax
        (require (only-in racket/base + λ module*))
        (require (for-syntax (only-in racket/base require for-syntax #%app begin-for-syntax #%datum only-in)))
        (+ 1 2)
        (λ (x) 3)
        (λ (x) 3)
        (λ (x) 3)
        (module* test4 racket 20)))))
