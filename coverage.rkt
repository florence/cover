(module coverage '#%kernel
  (#%provide coverage cover-name)
  (define-values (cover-name) (quote-syntax coverage))
  (define-values (coverage) (make-hash)))
