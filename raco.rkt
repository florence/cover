#lang racket/base
(require racket/list racket/cmdline raco/command-name
         setup/getinfo
         racket/match
         racket/contract/base
         racket/function
         "cover.rkt"
         (only-in "private/contracts.rkt" coverage-gen/c)
         "private/shared.rkt"
         "private/file-utils.rkt"
         (only-in (submod compiler/commands/test paths) collection-paths)
         pkg/lib)


(module+ test
  (require rackunit racket/runtime-path racket/set))

(module+ main

  (define coverage-dir "coverage")
  (define output-format "html")
  (define exclude-paths '())
  (define include-exts '())
  (define submod 'test)
  (define expansion-type 'dir)

  (define args
     (command-line
      #:program (short-program+command-name)
      #:once-each
      [("-d" "--directory") d
       "Specify output directory. Defaults to ./coverage."
       (set! coverage-dir d)]
      [("-f" "--format") format
       "Specify that coverage should be run and optional what format. Defaults to html."
       (set! output-format format)]
      [("-v" "--verbose")
       "Verbose mode"
       (verbose #t)]
      [("-b" "--exclude-pkg-basics")
        "exclude info.rkt, the tests directory, and the scribblings directory from the coverage report"
        (set! exclude-paths (append '("info.rkt" "tests" "scribblings") exclude-paths))]
      #:multi
      [("-n" "--no-output-for-path") t
       "exclude any paths named this from the coverage report."
       (set! exclude-paths (cons t exclude-paths))]
      [("-i" "--include-extensions") f
       "include these extensions in files to cover."
       (set! include-exts (cons f include-exts))]
      [("-s" "--submodule") s
       "Run the given submodule instead of the test submodule"
       (set! submod (string->symbol s))]
      #:once-any
      [("-c" "--collection") "Interprets the arguments as collections whose content should be tested (in the same way as directory content)."
       (set! expansion-type 'collection)]
      [("-p" "--package") "Interprets the arguments as packages whose contents should be tested (in the same way as directory content)."
       (set! expansion-type 'package)]
      #:args (file . files)
      (cons file files)))
  (define path-expand
    (case expansion-type
      [(dir) expand-directories]
      [(collection) (lambda (a b) (expand-directories (flatten (map collection-paths a)) b))]
      [(package) (lambda (a b)
                   (expand-directories (map pkg-directory a) b))]))
  (define files (path-expand args include-exts))
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
  (define paths+vectors
    (flatten
     (for/list ([f (in-list files)])
       (if (not (directory-exists? f))
           (->absolute f)
           (parameterize ([current-directory
                           (if (absolute-path? f)
                               f
                               (build-path (current-directory) f))])
             (expand-directory (append extensions comped)))))))
  (let loop ([paths paths+vectors])
    (match paths
      [(list) null]
      [(list x) (list x)]
      [(list* a (? vector? b) r)
       (cons (list a b) (loop r))]
      [(list* a r)
       (cons a (loop r))])))

(module+ test
  (define-runtime-path root ".")
  (define-runtime-path private "private")
  (define-runtime-path main.rkt "main.rkt")
  (define out
    (set "main.rkt"
         "private/coveralls.rkt"
         "private/contracts.rkt"
         "private/html.rkt"
         "private/format-utils.rkt"
         "private/file-utils.rkt"
         "private/shared.rkt"
         "private/raw.rkt"))
  (define (do-test ->)
    (parameterize ([current-directory root])
    (check-equal? (list->set
                   (map (compose path->string ->relative)
                        (expand-directories (list (path->string main.rkt)
                                                  (->(path->string private))))))
                  out)))
  (do-test ->relative)
  (do-test ->absolute))

;; -> (HorribyNestedListsOf (or PathString (list path-string vector))
(define (expand-directory exts [omit-paths null] [args null])
  (define new-omits (get-new-omits))
  (define full-omits (append new-omits omit-paths))
  (define new-argv (get-info-var (current-directory) 'test-command-line-arguments))
  (define expanded-argv
    (if (not new-argv)
        null
        (map (lambda (x)
               (list (->absolute (car x))
                     (list->vector (cadr x))))
             new-argv)))
  (define full-argv (append expanded-argv args))
  (if (should-omit? (current-directory) full-omits)
      null
      (for/list ([p (in-list (directory-list))])
        (cond [(directory-exists? p)
               (parameterize ([current-directory (build-path (current-directory) p)])
                 (expand-directory exts full-omits full-argv))]
              [(ormap (lambda (r) (regexp-match r (path->string p))) exts)
               (define path (path->string (build-path (current-directory) p)))
               (if (should-omit? path full-omits) null (path-add-argv path full-argv))]
              [else null]))))
(module+ test
  (define-runtime-path cur ".")
  (parameterize ([current-directory (build-path cur "tests/basic")])
    (check-equal? (list->set (map (compose path->string ->relative)
                                  (flatten (expand-directory extensions))))
                  (set "prog.rkt"
                       "not-run.rkt")))
  (parameterize ([current-directory cur])
    (define omit (map ->absolute (get-info-var cur 'test-omit-paths)))
    (define dirs (map ->absolute (filter list? (flatten (expand-directory extensions)))))
    (for ([o omit])
      (check-false (member o dirs)
                   (format "~s ~s" o dirs)))))

(define (get-new-omits)
  (append (get-omits 'test-omit-paths)
          (get-omits 'cover-omit-paths)))
(define (get-omits s)
  (define new-omits (get-info-var (current-directory) s))
  (case new-omits
    [(#f) null]
    [(all) (->absolute (current-directory))]
    [else (map ->absolute new-omits)]))

(define (path-add-argv path argvs)
  (define x (assoc path argvs))
  (or x path))

;; path symbol -> any
(define (get-info-var path sym)
  (define f (get-info/full/skip path))
  (and f (f sym (const #f))))

;; path (listof absolute-paths) -> boolean
(define (should-omit? path omits)
  (define epath (explode-path (->absolute path)))
  (for/or ([o omits])
    (define eo (explode-path (->absolute o)))
    (let loop ([eo eo] [ep epath])
      (cond [(and (null? eo) (null? ep)) #t]
            [(null? eo) #t]
            [(null? ep) #f]
            [(equal? (car eo) (car ep))
             (loop (cdr eo) (cdr ep))]
            [else #f]))))

(module+ test
  (check-true (should-omit? "/Test/t.rkt" '("/Test")))
  (check-true (should-omit? "/Test/t.rkt" '("/Test/t.rkt")))
  (check-true (should-omit? "/Users/florence/playground/cover/tests/error-file.rkt"
                            '("/Users/florence/playground/cover/tests/error-file.rkt")))
  (check-false (should-omit? "/Test/t.rkt" '("/OtherDir"))))

;; Coverage -> Coverage
(define (remove-excluded-paths cover paths)
  (for/hash ([(k v) (in-hash cover)]
             #:unless (and (is-excluded-path? k paths)
                           (vprintf "excluding path ~s from output\n" k)))
    (vprintf "including path ~s in output\n" k)
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
  (error 'cover "unable to load coverage format from ~s. Found unusable value ~s" dir v))

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
