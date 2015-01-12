#lang racket/base
(require racket/list racket/cmdline raco/command-name
         setup/getinfo
         racket/match
         racket/contract/base
         racket/function
         "main.rkt"
         (only-in "private/contracts.rkt" coverage-gen/c)
         "private/shared.rkt")

(module+ test
  (require rackunit racket/runtime-path racket/set))

(module+ main

  (define coverage-dir "coverage")
  (define output-format "html")
  (define exclude-paths '())
  (define include-exts '())
  (define submod 'test)

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
      [("-b" "--exclude-pkg-basics")
        "exclude info.rkt, the tests directory, and the scribblings directory from the coverage report"
        (set! exclude-paths (append '("info.rkt" "tests" "scribblings") exclude-paths))]
      #:multi
      [("-e" "--exclude-from-output") t
       "exclude any paths named this from the coverage report."
       (set! exclude-paths (cons t exclude-paths))]
      [("-i" "--include-extensions") f
       "include these extensions in files to cover."
       (set! include-exts (cons f include-exts))]
      [("-s" "--submodule") s
       "Run the given submodule instead of the test submodule"
       (set! submod (string->symbol s))]
      #:args (file . files)
      (cons file files)))
  (define files (expand-directories args include-exts))
  (define generate-coverage
    (hash-ref (get-formats) output-format
              (lambda _ (error 'cover "given unknown coverage output format: ~s" output-format))))
  (printf "generating test coverage for ~s\n" files)
  (define passed (keyword-apply test-files! '(#:submod) (list submod) files))
  (define coverage (remove-excluded-paths (get-test-coverage) exclude-paths))
  (printf "dumping coverage info into ~s\n" coverage-dir)
  (generate-coverage coverage coverage-dir)
  (unless passed
    (printf "some tests failed\n")))

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

(module+ test
  (define-runtime-path root".")
  (define-runtime-path private "private")
  (define-runtime-path main.rkt "main.rkt")
  (parameterize ([current-directory root])
    (check-equal? (list->set
                   (map (compose path->string ->relative)
                        (expand-directories (list (path->string main.rkt)
                                                  (->relative (path->string private))))))
                  (set "main.rkt"
                       "private/coveralls.rkt"
                       "private/contracts.rkt"
                       "private/html.rkt"
                       "private/format-utils.rkt"
                       "private/shared.rkt"
                       "private/raw.rkt"))))

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


(define (get-formats)
  (define dirs (find-relevant-directories '(cover-formats) 'all-available))
  (for*/hash ([d (in-list dirs)]
              [f (in-value (get-info/full/skip d))]
              #:when f
              [v (in-value (f 'cover-formats (const #f)))]
              #:when v
              [l (in-list v)])
    (with-handlers ([exn:misc:match? (make-cover-load-error d l)])
      (match-define (list (? string? name) (? module-path? path) (? symbol? ident)) l)
      (define f (dynamic-require path ident (make-cover-require-error ident path)))
      (values
       name
       (contract coverage-gen/c f 'cover ident ident #f)))))

(define ((make-cover-load-error dir v) . _)
  (error 'cover "unable to load coverage format from ~s. Found unusable value ~s"
         dir v))
(define ((make-cover-require-error ident path))
  (error 'cover "unable to load symbol ~s from ~s" ident path))

(define (get-info/full/skip dir)
  (with-handlers ([exn:fail? (const #f)])
    (get-info/full dir)))

(module+ test
  (test-begin
   ;; we expect that a standard install has "html", "coveralls", and "raw"
   (define h (get-formats))
   (check-true (hash-has-key? h "html"))
   (check-true (hash-has-key? h "coveralls"))
   (check-true (hash-has-key? h "raw"))))
