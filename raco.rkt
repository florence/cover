#lang racket
(require raco/command-name "main.rkt")

(module+ main
  (define files
    (expand-directories
     (command-line
      #:program (short-program+command-name)
      #:args files files)))
  (apply test-files! files))

;; TODO allow for arbitrary extensions
(define extensions '(#rx".rkt$" #rx".ss$"))
(define (expand-directories files)
  (flatten
   (for/list ([f files])
     (if (not (directory-exists? f))
         f
         (expand-directory f)))))

(define (expand-directory d)
  (for/list ([p (directory-list d)])
    (cond [(directory-exists? p)
           (parameterize ([current-directory (build-path (current-directory) p)])
             (expand-directory "."))]
          [(ormap (lambda (r) (regexp-match r (path->string p))) extensions)
           (path->string (build-path (current-directory) p))]
          [else (displayln " is bad file") null])))
