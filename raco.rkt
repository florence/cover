#lang racket
(require raco/command-name "cover.rkt" "format.rkt" "private/shared.rkt")
(module+ test
  (require rackunit racket/runtime-path))

(module+ main

  (define coverage-dir "coverage")
  (define output-format "html")
  (define exclude-paths '("info.rkt"))
  (define include-exts '())

  (define args
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
       "exclude all paths named this from the coverage report. By default excludes paths named tests"
       (set! exclude-paths (cons t exclude-paths))]
      [("-i" "--include-extentions") f
       "include these extentions in files to cover."
       (set! include-exts (cons f include-exts))]
      #:args (file . files)
      (cons file files)))
  (define files (expand-directories args include-exts))
  (define generate-coverage
    (case output-format
      [("html") generate-html-coverage]
      [("coveralls") generate-coveralls-coverage]
      [("raw") generate-raw-coverage]
      [else (error 'cover "given unknown coverage output format: ~s" output-format)]))
  (printf "generating test coverage for ~s\n" files)
  (define passed (apply test-files! files))
  (define coverage (remove-excluded-paths (get-test-coverage) exclude-paths))
  (printf "dumping coverage info into ~s\n" coverage-dir)
  (generate-coverage coverage coverage-dir)
  (exit
   (case passed
     [(#t) 0]
     [(#f) 1])))

;; TODO allow for arbitrary extensions
(define extensions '(#rx"\\.rkt$" #rx"\\.ss$"))
(define (expand-directories files [exts null])
  (define comped (map regexp exts))
  (flatten
   (for/list ([f files])
     (if (not (directory-exists? f))
         f
         (parameterize ([current-directory
                         (if (absolute-path? f)
                             f
                             (build-path (current-directory) f))])
           (expand-directory (append extensions comped)))))))

;; -> (HorribyNestedListsOf PathString)
(define (expand-directory exts)
  (for/list ([p (directory-list)])
    (cond [(directory-exists? p)
           (parameterize ([current-directory (build-path (current-directory) p)])
             (expand-directory exts))]
          [(ormap (lambda (r) (regexp-match r (path->string p))) exts)
           (path->string (build-path (current-directory) p))]
          [else null])))
(module+ test
  (define-runtime-path cur ".")
  (parameterize ([current-directory (build-path cur "tests/basic")])
    (check-equal? (list->set (map (compose path->string ->relative)
                                  (flatten (expand-directory extensions))))
                  (set "prog.rkt"
                       "not-run.rkt"))))

;; Coverage -> Coverage
(define (remove-excluded-paths cover paths)
  (for/hash ([(k v) cover]
             #:unless (is-excluded-path? k paths))
    (values k v)))

(module+ test
  (parameterize ([current-directory (build-path "/tests")])
    (check-equal? (remove-excluded-paths 
                   (hash "/tests/tests/x.rkt" null
                         "/tests/x/tests/x/x.rkt" null
                         "/tests/x.rkt" null)
                   '("tests"))
                  (hash "/tests/x.rkt" null))))


;; PathString [ListOf PathString]-> any/c
(define (is-excluded-path? k paths)
  (define expl (explode-path (->relative k)))
  (ormap (lambda (d) (member (build-path d) expl))
         paths))

(module+ test
  (parameterize ([current-directory (build-path "/tests")])
    (check-not-false (is-excluded-path? "/test/test/x.rkt" '("test")))
    (check-false (is-excluded-path? "/test/x.rkt" '("test")))
    (check-false (is-excluded-path? "/test/t/x.rkt" '("test")))))

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
