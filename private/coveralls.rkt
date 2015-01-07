#lang racket
(provide generate-coveralls-coverage)
(require racket/runtime-path json "format-utils.rkt" "shared.rkt" racket/pretty)

(module+ test
  (require rackunit "../cover.rkt" racket/runtime-path))

;; Coveralls

;; Coverage [path-string] -> Void
(define-runtime-path post "curl.sh")
(define (generate-coveralls-coverage coverage [dir "coverage"])
  (make-directory* dir)
  (define coverage-path dir)
  (define coverage-file (build-path coverage-path "coverage.json"))
  (define json (generate-coveralls-json coverage (hasheq)))
  (define meta-data (determine-build-type))
  (define meta-with-git-info (hash-merge meta-data (get-git-info)))
  (define data (hash-merge json meta-with-git-info))
  (vprintf "writing json to file ~s\n" coverage-file)
  (with-output-to-file coverage-file
    (thunk (write-json data))
    #:exists 'replace)
  (when (verbose)
    (printf "data written was:\n")
    (pretty-print data))
  (vprintf "invoking coveralls API")
  (parameterize ([current-output-port
                  (if (verbose)
                      (current-output-port)
                      (open-output-nowhere))])
    (void (system* (path->string post)
                   coverage-file
                   (if (verbose) "-v" "")))))

;; Maps service name to the environment variable that indicates that the service is to be used.
(define BUILD-TYPES (hash "travis-ci" "TRAVIS_JOB_ID"))

;; -> [Hasheq String String
;; Determine the type of build (e.g. repo token, travis, etc) and return the appropriate metadata
(define (determine-build-type)
  (define service-name (for/first ([(name var) BUILD-TYPES] #:when (getenv var)) name))
  (define repo-token (getenv "COVERALLS_REPO_TOKEN"))
  (vprintf "using repo token: ~s\n" repo-token)
  (vprintf "using service name: ~s\n" service-name)
  (cond [service-name
         (hasheq 'service_name service-name
                 'service_job_id (getenv (hash-ref BUILD-TYPES service-name))
                 'repo_token repo-token)]
        [repo-token (hasheq 'service_name "cover" 'repo_token repo-token)]
        [else (error "No repo token or ci service detected")]))
(module+ test
  (define-syntax (with-env stx)
    (syntax-case stx ()
      [(test-with-env (env ...) test ...)
       #'(parameterize ([current-environment-variables
                         (make-environment-variables
                          (string->bytes/utf-8 env) ...)])
           test ...)]))
  (with-env ()
    (check-exn void determine-build-type))
  (with-env ("COVERALLS_REPO_TOKEN" "abc")
    (check-equal? (determine-build-type)
                  (hasheq 'service_name "cover"
                          'repo_token "abc")))
  (with-env ("TRAVIS_JOB_ID" "abc")
    (check-equal? (determine-build-type)
                  (hasheq 'service_name "travis-ci"
                          'service_job_id "abc"
                          'repo_token #f))))

;; Coverage [Hasheq String String] -> JSexpr
;; Generates a string that represents a valid coveralls json_file object
(define (generate-coveralls-json coverage meta)
  (define src-files
    (for/list ([file (hash-keys coverage)])
      (define local-file (path->string (find-relative-path (current-directory) file)))
      (define src (file->string file))
      (define c (line-coverage coverage file))
      (hasheq 'source src 'coverage c 'name local-file)))
  (hash-set meta 'source_files src-files))

(module+ test
  (define-runtime-path tests/prog.rkt"../tests/prog.rkt")
  (define-runtime-path root "..")
  (test-begin
   (parameterize ([current-directory root])
     (after
      (define file (path->string (simplify-path tests/prog.rkt)))
      (test-files! (path->string (simplify-path tests/prog.rkt)))
      (define coverage (get-test-coverage))
      (check-equal?
       (generate-coveralls-json coverage (hasheq))
       (hasheq 'source_files
               (list (hasheq 'source (file->string tests/prog.rkt)
                             'coverage (line-coverage coverage file)
                             'name "tests/prog.rkt"))))
      (clear-coverage!)))))

;; CoverallsCoverage = Nat | json-null

;; Coverage PathString Covered? -> [Listof CoverallsCoverage]
;; Get the line coverage for the file to generate a coverage report
(define (line-coverage coverage file)
  (define covered? (make-covered? (hash-ref coverage file) file))
  (define split-src (string-split (file->string file) "\n"))
  (define file-coverage (hash-ref coverage file))
  (define (process-coverage value rst-of-line)
    (case (covered? value)
      ['yes (if (equal? 'no rst-of-line) rst-of-line 'yes)]
      ['no 'no]
      [else rst-of-line]))
  (define (process-coverage-value value)
    (case value
      ['yes 1]
      ['no 0]
      [else (json-null)]))

  (define-values (line-cover _)
    (for/fold ([coverage '()] [count 1]) ([line split-src])
      (cond [(zero? (string-length line)) (values (cons (json-null) coverage) (add1 count))]
            [else (define nw-count (+ count (string-length line) 1))
                  (define all-covered (foldr process-coverage 'missing (range count nw-count)))
                  (values (cons (process-coverage-value all-covered) coverage) nw-count)])))
  (reverse line-cover))

(module+ test
  (define-runtime-path path "../tests/basic/not-run.rkt")
  (let ()
    (define file (path->string (simplify-path path)))
    (test-files! file)
    (check-equal? (line-coverage (get-test-coverage) file) '(1 0))
    (clear-coverage!)))

(define (hash-merge h1 h2) (for/fold ([res h1]) ([(k v) h2]) (hash-set res k v)))


;; Git Magic

(define (get-git-info)
  (hasheq 'git
          (hasheq 'head (get-git-commit)
                  'branch (get-git-branch)
                  'remotes (get-git-remotes))))

(define (get-git-branch)
  (string-trim
   (or (getenv "TRAVIS_BRANCH")
       (with-output-to-string (thunk (system "git rev-parse --abbrev-ref HEAD"))))))

(define (get-git-remotes)
  (parse-git-remote (with-output-to-string (thunk (system "git remote -v")))))
(define (parse-git-remote raw)
  (define lines (string-split raw "\n"))
  (define fetch-only (filter (λ (line) (regexp-match #rx"\\(fetch\\)" line)) lines))
  (for/list ([line fetch-only])
    (define split (string-split line))
    (hasheq 'name (list-ref split 0)
            'url (list-ref split 1))))
(module+ test
  (test-begin
   (define raw
     "origin	git@github.com:florence/cover.git (fetch)\norigin	git@github.com:florence/cover.git (push)")
   (check-equal? (parse-git-remote raw)
                 (list (hasheq 'name "origin"
                               'url "git@github.com:florence/cover.git")))))

(define (get-git-commit)
  (define format (string-join '("%H" "%aN" "%ae" "%cN" "%ce" "%s") "%n"))
  (define command (string-append "git --no-pager log -1 --pretty=format:" format))
  (define log (with-output-to-string (thunk (system command))))
  (define lines (string-split log "\n"))
  (for/hasheq ([field '(id author_name author_email committer_name committer_email message)]
               [line lines])
    (values field line)))
