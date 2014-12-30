#lang racket
(require raco/command-name "cover.rkt" "format.rkt" "private/shared.rkt")

(module+ main

  (define coverage-dir "coverage")
  (define output-format "html")

  (define files
    (expand-directories
     (command-line
      #:program (short-program+command-name)
      #:once-each
      [("-d" "--directory") d
       "Specify output directory. Defaults to ./coverage."
       (set! coverage-dir d)]
      [("-c" "--coverage") format
       "Specify that coverage should be run and optional what format. Defaults to html."
       (set! output-format format)]
      [("-v" "--verbose")
       "Verbose mode"
       (verbose #t)]
      #:args (file . files)
      (cons file files))))
  (define generate-coverage
    (case output-format
      [("html") generate-html-coverage]
      [("coveralls") generate-coveralls-coverage]
      [("raw") generate-raw-coverage]
      [else (error 'cover "given unknown coverage output format: ~s" output-format)]))
  (printf "generating test coverage for ~s\n" files)
  (define passed (apply test-files! files))
  (define coverage (get-test-coverage))
  (printf "dumbing coverage info into ~s\n" coverage-dir)
  (generate-coverage coverage coverage-dir)
  (exit
   (case passed
     [(#t) 0]
     [(#f) 1])))

;; TODO allow for arbitrary extensions
(define extensions '(#rx"\\.rkt$" #rx"\\.ss$"))
(define (expand-directories files)
  (flatten
   (for/list ([f files])
     (if (not (directory-exists? f))
         f
         (parameterize ([current-directory
                         (if (absolute-path? f)
                             f
                             (build-path (current-directory) f))])
           (expand-directory))))))

(define (expand-directory)
  (for/list ([p (directory-list)])
    (cond [(directory-exists? p)
           (parameterize ([current-directory (build-path (current-directory) p)])
             (expand-directory))]
          [(ormap (lambda (r) (regexp-match r (path->string p))) extensions)
           (path->string (build-path (current-directory) p))]
          [else null])))
