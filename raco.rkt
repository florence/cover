#lang racket
(require raco/command-name "main.rkt" "format.rkt")

(module+ main

  (define coverage-dir "coverage")
  (define coverage? #f)
  (define output-format "")

  (define files
    (expand-directories
     (command-line
      #:program (short-program+command-name)
      #:once-any
      [("-d" "--directory") d "Specify output directory" (set! coverage-dir d)]
      [("-c" "--coverage") format
                           "Specify that coverage should be run and optional what format"
                           (set! coverage? #t)
                           (set! output-format format)]
      #:args files
      files)))
  (printf "testing ~s\n" files)
  (define passed (keyword-apply test-files! '(#:coverage) (list coverage?) files))
  (when coverage?
    (printf "COVERAGE!")
    (define coverage (get-test-coverage))
    (case output-format
      [("html") (generate-html-coverage coverage coverage-dir)]
      [("coveralls") (generate-coveralls-coverage coverage (hasheq) coverage-dir)]))
  (exit
   (case passed
     [(#t) 0]
     [(#f) 1])))

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
