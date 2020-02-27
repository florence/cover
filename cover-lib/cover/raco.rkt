#lang racket/base
(require racket/list racket/cmdline raco/command-name
         setup/getinfo
         compiler/module-suffix
         racket/match
         racket/contract/base
         racket/function
         racket/logging
         cover
         (only-in cover/private/contracts coverage-gen/c)
         cover/private/shared
         cover/private/file-utils
         (only-in (submod compiler/commands/test paths) collection-paths)
         racket/path
         pkg/lib
         racket/port
         (for-syntax racket/base syntax/parse))
(provide get-files)

(module+ main

  (define coverage-dir "coverage")
  (define output-formats "html")
  (define exclude-paths '())
  (define include-exts '())
  (define submods 'test)
  (define expansion-type 'dir)
  (define irrel-submods #f)
  (define noise 'normal)
  (define quite #t)
  (define supress-log-execution #f)

  (define args
    (command-line
     #:program (short-program+command-name)
     #:once-each
     [("-d" "--directory") d
                           "Specify output directory. Defaults to ./coverage."
                           (set! coverage-dir d)]
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
     #:once-any
     [("-v" "--verbose")
      "Verbose mode"
      (set! noise 'verbose)]
     [("-Q" "--quiqt")
      "quiet mode"
      (set! noise 'quiet)]
     #:args (file . files)
     (cons file files)))
  (with-logging-to-port
   (case noise
     [(verbose) (current-error-port)]
     [(normal) (current-output-port)]
     [(quiet) (open-output-nowhere)])
   #:logger (current-logger)
   (lambda ()
     (define-values (files cleaned-files)
       (get-files expansion-type args include-exts exclude-paths))
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
     (log-cover-info "dumping coverage info into ~s" coverage-dir)
     (parameterize ([irrelevant-submodules irrel-submods])
       (generate-coverage coverage
                          (for/list ([f cleaned-files])
                            (cond
                              [(path? f) (path->string f)]
                              [else f]))
                          coverage-dir))
     (unless passed
       (log-cover-error "some tests failed")))
   (case noise
     [(verbose) 'debug]
     [(normal) 'info]
     [(quiet) 'none])
   'cover))


(define (get-files expansion-type args include-exts exclude-paths)
  (define path-expand
    (case expansion-type
      [(dir) expand-directories]
      [(file) filter-exts]
      [(lib) expand-lib]
      [(collection)
       (lambda (a b)
         (expand-directories
          (flatten
           (map ensure-collection-exists (map collection-paths a) a))
          b))]
      [(package)
       (lambda (a b)
         (expand-directories
          (map ensure-pkg-exists (map pkg-directory a) a)
          b))]))
  (define files (path-expand args include-exts))
  (define cleaned-files (remove-excluded-paths files exclude-paths))
  (values files cleaned-files))

(define (expand-lib files [exts null])
  (define (find x)
    (define rmp ((current-module-name-resolver) x #f #f #f))
    (define p (resolved-module-path-name rmp))
    (and (file-exists? p) p))
  (for/list ([f files])
    (match (find `(lib ,f))
      [#f (error 'cover "module not found: ~a" f)]
      [l l])))

;; given a (U path-string? false?) representing
;; the result of calling pkg-directory on a string and
;; the name of the specified package,
;; ensure that the argument is a path-string and not a #f
(define (ensure-pkg-exists path-str pkg-name)
  (or path-str
      (error 'cover "no such installed package: ~v" pkg-name)))

;; given a (treeof path-string?) representing
;; the result of calling collection-paths
;; ensure that the argument is non-empty
(define (ensure-collection-exists tree collection-name)
  (or (and (cons? tree) tree)
      (error 'cover "no such collection: ~a" collection-name)))


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

;; -> (HorribyNestedListsOf (or PathString (list path-string vector))
(define (expand-directory inc-paths [omit-paths null] [args null])
  (define new-omits (get-new-omits))
  (define full-omits (append new-omits omit-paths))
  (define new-incs (get-new-incs))
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
    [(all) (list (->absolute (current-directory)))]
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


;; (listof (U path (list Path Vector)) (listof path) -> (listof path-string)
(define (remove-excluded-paths files paths)
  (define (->path p) (if (path-string? p) p (first p)))
  (for/list ([k (in-list (map ->path files))]
             #:unless (and (is-excluded-path? k paths)
                           (log-cover-debug "excluding path ~s from output\n" k)))
    (log-cover-debug "including path ~s in output\n" k)
    k))

;; PathString [ListOf PathString]-> any/c
(define (is-excluded-path? k paths)
  (define expl (explode-path (->relative k)))
  (ormap (lambda (d) (member (build-path d) expl))
         paths))

(define (get-formats)
  (define dirs (find-relevant-directories '(cover-formats) 'all-available))
  (hash-set
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
        (contract coverage-gen/c f 'cover ident ident #f))))
   "none" (lambda _ (void))))

(define ((make-cover-load-error dir v) . _)
  (error 'cover "unable to load coverage format from ~s. Found unusable value ~s" dir v))

(define ((make-cover-require-error ident path))
  (error 'cover "unable to load symbol ~s from ~s" ident path))

(define (get-info/full/skip dir)
  (with-handlers ([exn:fail? (const #f)])
    (get-info/full dir)))
