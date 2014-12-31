#lang racket
(require raco/command-name "cover.rkt" "format.rkt" "private/shared.rkt")
(module+ test
  (require rackunit racket/runtime-path))

(module+ main

  (define coverage-dir "coverage")
  (define output-format "html")
  (define exclude-dirs '())

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
      #:multi
      [("-e" "--exclude-from-output") t
       "exclude all directories named this from the coverage report. By default excludes dirs named tests"
       (set! exclude-dirs (cons t exclude-dirs))]
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
  (define coverage (remove-dirs (get-test-coverage) exclude-dirs))
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

;; -> (HorribyNestedListsOf PathString)
(define (expand-directory)
  (for/list ([p (directory-list)])
    (cond [(directory-exists? p)
           (parameterize ([current-directory (build-path (current-directory) p)])
             (expand-directory))]
          [(ormap (lambda (r) (regexp-match r (path->string p))) extensions)
           (path->string (build-path (current-directory) p))]
          [else null])))
(module+ test
  (define-runtime-path cur ".")
  (parameterize ([current-directory (build-path cur "tests/basic")])
    (check-equal? (list->set (map (compose path->string ->relative)
                                  (flatten (expand-directory))))
                  (set "prog.rkt"
                       "not-run.rkt"))))

;; Coverage -> Coverage
(define (remove-dirs cover dirs)
  (for/hash ([(k v) cover]
             #:unless (is-dir? k dirs))
    (values k v)))
(module+ test
  (parameterize ([current-directory (build-path "/tests")])
    (check-equal? (remove-dirs (hash "/tests/tests/x.rkt" null
                                     "/tests/x/tests/x/x.rkt" null
                                     "/tests/x.rkt" null)
                               '("tests"))
                  (hash "/tests/x.rkt" null))))


;; PathString -> any/c
(define (is-dir? k dirs)
  (define expl (explode-path (->relative k)))
  (ormap (lambda (d) (member (build-path d) expl))
         dirs))
(module+ test
  (parameterize ([current-directory (build-path "/tests")])
    (check-not-false (is-dir? "/test/test/x.rkt" '("test")))
    (check-false (is-dir? "/test/x.rkt" '("test")))
    (check-false (is-dir? "/test/t/x.rkt" '("test")))))

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
