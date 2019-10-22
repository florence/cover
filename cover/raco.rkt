#lang racket/base
(require racket/list racket/cmdline raco/command-name
         setup/getinfo
         compiler/module-suffix
         racket/match
         racket/contract/base
         racket/function
         racket/logging
         "main.rkt"
         (only-in "private/contracts.rkt" coverage-gen/c)
         "private/shared.rkt"
         "private/file-utils.rkt"
         (only-in (submod compiler/commands/test paths) collection-paths)
         racket/path
         pkg/lib
         racket/port
         (for-syntax racket/base syntax/parse))


(module+ test
  (require rackunit racket/runtime-path racket/set))

(module+ main

  (define coverage-dir "coverage")
  (define output-formats "html")
  (define exclude-paths '())
  (define include-exts '())
  (define submods 'test)
  (define expansion-type 'dir)
  (define irrel-submods #f)
  (define verbose #f)
  (define supress-log-execution #f)

  (define args
     (command-line
      #:program (short-program+command-name)
      #:once-each
      [("-d" "--directory") d
       "Specify output directory. Defaults to ./coverage."
       (set! coverage-dir d)]
      [("-v" "--verbose")
       "Verbose mode"
       (set! verbose #t)]
      [("-b" "--exclude-pkg-basics")
        "exclude info.rkt, the tests directory, and the scribblings directory from the coverage report"
        (set! exclude-paths (list* "info.rkt" "tests" "scribblings" exclude-paths))]
      [("--suppress-log-execution")
       "Stop cover from executing all logging statements."
       (set! supress-log-execution #t)]
      #:multi
      [("-f" "--format") format
       "Specify that coverage should be run and optionally what formats. Defaults to html."
       (set! output-formats (cons format (if (list? output-formats) output-formats '())))]
      [("-n" "--no-output-for-path") t
       "exclude any paths named this from the coverage report."
       (set! exclude-paths (cons t exclude-paths))]
      [("-i" "--include-extensions") f
       "include these extensions in files to cover. Accepts regular expressions"
       (set! include-exts (cons f include-exts))]
      [("-s" "--submodule") s
       "Run the given submodule instead of the test submodule."
       (set! submods (cons (string->symbol s) (if (list? submods) submods '())))]
      [("-e" "--irrelevant-submodules") s
       "Consider the given submodules irrelevant when generating coverage. If not provided defaults to all submodules."
       (unless irrel-submods
         (set! irrel-submods null))
       (set! irrel-submods (cons (string->symbol s) irrel-submods))]
      #:once-any
      [("-c" "--collection")
       ("Interprets the arguments as collections whose content"
        " should be tested (in the same way as directory content).")
       (set! expansion-type 'collection)]
      [("-p" "--package")
       ("Interprets the arguments as packages whose contents"
        " should be tested (in the same way as directory content).")
       (set! expansion-type 'package)]
      [("-m" "--modules")
       ("Interpret arguments as modules"
        "  (ignore argument unless \".rkt\", \".scrbl\")")
       (set! expansion-type 'file)]
      [("-l" "--lib")
       "Interperet arguments as libraries"
       (set! expansion-type 'lib)]
      #:args (file . files)
      (cons file files)))
  (with-logging-to-port
      (if verbose
          (current-error-port)
          (open-output-nowhere))
    (lambda ()
      (define path-expand
        (case expansion-type
          [(dir) expand-directories]
          [(file) filter-exts]
          [(lib) expand-lib]
          [(collection) (lambda (a b) (expand-directories (flatten (map collection-paths a)) b))]
          [(package) (lambda (a b)
                       (expand-directories (map pkg-directory a) b))]))
      (define files (path-expand args include-exts))
      (define cleaned-files (remove-excluded-paths files exclude-paths))
      (define (generate-coverage . args)
        (for/list ([output-format (in-list (if (list? output-formats)
                                               output-formats
                                               (list output-formats)))])
          (apply (hash-ref (get-formats) output-format
                           (lambda _
                             (error 'cover "given unknown coverage output format: ~s" output-format)))
                 args)))
      (define (exec)
        (apply test-files!
               #:submod submods
               #:dont-compile exclude-paths
               files))
      (define passed
        (cond
          [supress-log-execution
           (exec)]
          [else 
           (with-intercepted-logging void exec 'debug)]))
      (define coverage (get-test-coverage))
      (printf "dumping coverage info into ~s\n" coverage-dir)
      (parameterize ([irrelevant-submodules irrel-submods])
        (generate-coverage coverage
                           (for/list ([f cleaned-files])
                             (cond
                               [(path? f) (path->string f)]
                               [else f]))
                           coverage-dir))
      (unless passed
        (printf "some tests failed\n")))
    'debug
    'cover))

(define (expand-lib files [exts null])
  (define (find x)
    (define rmp ((current-module-name-resolver) x #f #f #f))
    (define p (resolved-module-path-name rmp))
    (and (file-exists? p) p))
  (for/list ([f files])
    (match (find `(lib ,f))
      [#f (error 'cover "module not found: ~a" f)]
      [l l])))

(module+ test
  (test-begin
   (define p (first (expand-lib '("racket/base"))))
   (check-not-false p)
   (check-true (file-exists? p))))

(define-syntax (maybe stx)
  (syntax-parse stx
    [(_) #'#t]
    [(_ [id:id e]) #'(let ([id e]) id)]
    [(_ [id:id e] b ...)
     #'(let ([id e])
         (and id (maybe b ...)))]))

(define (filter-exts files [exts null])
  (for/list ([f files]
             #:when (maybe [ext? (filename-extension f)]
                           [ext (bytes->string/locale ext?)]
                           [res (regexp-match (get-module-suffix-regexp) (string-append "." ext))]))
    f))
(module+ test
  (check-equal? (filter-exts '("a.rkt" "b.rkt" "c/d/e.scrbl" "a/b/c" "a/b.qqq"))
                '("a.rkt" "b.rkt" "c/d/e.scrbl")))

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
             (expand-directory (cons (get-module-suffix-regexp) comped)))))))
  (let loop ([paths paths+vectors])
    (match paths
      [(list) null]
      [(list* a (? vector? b) r)
       (cons (list a b) (loop r))]
      [(list* a r)
       (cons a (loop r))])))

(module+ test
  (define extensions #px#"^(.*)\\.(?i:rkt|scm|scrbl|ss)$")
  (define-runtime-path root ".")
  (define-runtime-path private "private")
  (define-runtime-path main.rkt "main.rkt")
  (define out
    (set "main.rkt"
         "private/contracts.rkt"
         "private/html/html.rkt"
         "private/format-utils.rkt"
         "private/file-utils.rkt"
         "private/shared.rkt"
         "private/raw.rkt"))
  (define (do-test ->)
    (parameterize ([current-directory root])
    (check-equal? (list->set
                   (map (compose path->string ->relative)
                        (expand-directories (list (path->string main.rkt)
                                                  (-> (path->string private))))))
                  out)))
  (do-test ->relative)
  (do-test ->absolute))

;; -> (HorribyNestedListsOf (or PathString (list path-string vector))
(define (expand-directory inc-paths [omit-paths null] [args null])
  (define new-omits (get-new-omits))
  (define full-omits (append new-omits omit-paths))
  (define new-incs (get-new-incs))
  (define full-incs (append new-incs inc-paths))
  (define new-argv (get-info-var (current-directory) 'test-command-line-arguments))
  (define expanded-argv
    (if (not new-argv)
        null
        (map (lambda (x)
               (list (->absolute (car x))
                     (vector->immutable-vector (list->vector (cadr x)))))
             new-argv)))
  (define full-argv (append expanded-argv args))
  (if (should-omit? (current-directory) full-omits)
      null
      (for/list ([p (in-list (directory-list))])
        (cond [(directory-exists? p)
               (parameterize ([current-directory (build-path (current-directory) p)])
                 (expand-directory inc-paths full-omits full-argv))]
              [(ormap (lambda (r) (regexp-match r (path->string p))) inc-paths)
               (define path (path->string (build-path (current-directory) p)))
               (if (should-omit? path full-omits) null (path-add-argv path full-argv))]
              [else null]))))
(module+ test
  (define-runtime-path cur ".")
  (parameterize ([current-directory (build-path cur "tests/basic")])
    (check-equal? (list->set (map (compose path->string ->relative)
                                  (flatten (expand-directory (list extensions)))))
                  (set "prog.rkt"
                       "not-run.rkt"
                       "raise.rkt"
                       "empty-ISL.rkt"
                       "no-expressions.rkt")))
  (parameterize ([current-directory cur])
    (define omit (map ->absolute (get-info-var cur 'test-omit-paths)))
    (define dirs (map ->absolute (filter list? (flatten (expand-directory (list extensions))))))
    (for ([o omit])
      (check-false (member o dirs)
                   (format "~s ~s" o dirs)))))

(define (get-new-omits)
  (append (get-omits/incs 'test-omit-paths)
          (get-omits/incs 'cover-omit-paths)))

(define (get-new-incs)
  (append (get-omits/incs 'test-include-paths)
          (get-omits/incs 'cover-include-paths)
          (get-omits/incs 'module-suffixes)))

(define (get-omits/incs s)
  (define new-omits (get-info-var (current-directory) s))
  (case new-omits
    [(#f) null]
    [(all) (->absolute (current-directory))]
    [else (map (lambda (x)
                 (cond [(regexp? x) x]
                       [(bytes? x) (regexp (bytes->string/locale x))]
                       [else (->absolute x)]))
               new-omits)]))

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
    (if (or (byte-regexp? o) (regexp? o))
        (regexp-match? o path)
        (let ([eo (explode-path (->absolute o))])
          (let loop ([eo eo] [ep epath])
            (cond [(and (null? eo) (null? ep)) #t]
                  [(null? eo) #t]
                  [(null? ep) #f]
                  [(equal? (car eo) (car ep))
                   (loop (cdr eo) (cdr ep))]
                  [else #f]))))))

(module+ test
  (check-true (should-omit? "/Test/t.rkt" '("/Test")))
  (check-true (should-omit? "/Test/t.rkt" '("/Test/t.rkt")))
  (check-true (should-omit? "/Users/florence/playground/cover/tests/error-file.rkt"
                            '("/Users/florence/playground/cover/tests/error-file.rkt")))
  (check-false (should-omit? "/Test/t.rkt" '("/OtherDir")))
  (check-true (should-omit? "/Users/florence/playground/cover/tests/error-file.rkt"
                            (list extensions)))
  (check-false (should-omit? "/Users/florence/playground/cover/tests/error-file.qq"
                             (list extensions))))

;; (listof (U path (list Path Vector)) (listof path) -> (listof path-string)
(define (remove-excluded-paths files paths)
  (define (->path p) (if (path-string? p) p (first p)))
  (for/list ([k (in-list (map ->path files))]
             #:unless (and (is-excluded-path? k paths)
                           (log-cover-debug "excluding path ~s from output\n" k)))
    (log-cover-debug "including path ~s in output\n" k)
    k))

(module+ test
  (parameterize ([current-directory (build-path "/tests")])
    (check-equal? (remove-excluded-paths
                   (list (list "/tests/tests/x.rkt" #())
                         "/tests/x/tests/x/x.rkt"
                         "/tests/x.rkt")
                   '("tests"))
                  (list "/tests/x.rkt"))))


;; PathString [ListOf PathString]-> any/c
(define (is-excluded-path? k paths)
  (define expl (explode-path (->relative k)))
  (ormap (lambda (d) (member (build-path d) expl))
         paths))

(module+ test
  (parameterize ([current-directory (build-path "/test")])
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
   (check-true (hash-has-key? h "raw"))))
