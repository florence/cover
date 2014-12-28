#lang racket
(require raco/command-name "main.rkt")

(module+ main
  (define files
    (expand-directories
     (command-line
      #:program (short-program+command-name)
      #:args files files)))
  (printf "testing ~s\n" files)
  (apply test-files! files))

;; TODO allow for arbitrary extensions
(define extensions '(#rx".rkt$" #rx".ss$"))
(define (expand-directories files)
  (flatten
   (for/list ([f files])
     (if (not (directory-exists? f))
         f
         (parameterize ([current-directory (build-path (current-directory) f)])
           (expand-directory))))))

(define (expand-directory)
  (for/list ([p (directory-list)])
    (cond [(directory-exists? p)
           (parameterize ([current-directory (build-path (current-directory) p)])
             (expand-directory))]
          [(ormap (lambda (r) (regexp-match r (path->string p))) extensions)
           (path->string (build-path (current-directory) p))]
          [else null])))
