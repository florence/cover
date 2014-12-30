#lang racket
(provide generate-coveralls-coverage)
(require racket/runtime-path json "format-utils.rkt" "shared.rkt")

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
  (define data (for/fold ([blob json]) ([(k v) meta-data]) (hash-set blob k v)))
  (with-output-to-file coverage-file
    (thunk (write-json data))
    #:exists 'replace)
  (when (verbose)
    (printf "\n\n\nwriting json to file ~s\n" dir)
    (write-json data (current-output-port))
    (printf "\n\n\n"))
  (system* (path->string post) coverage-file))

;; Maps service name to the environment variable that indicates that the service is to be used.
(define BUILD-TYPES (hash "travis-ci" "TRAVIS_BUILD_ID"))

;; -> [Hasheq String String
;; Determine the type of build (e.g. repo token, travis, etc) and return the appropriate metadata
(define (determine-build-type)
  (define service-name (for/first ([(name var) BUILD-TYPES] #:when (getenv var)) name))
  (define repo-token (getenv "COVERALLS_REPO_TOKEN"))
  (cond [service-name
         (hasheq 'service_name service-name
                 'service_job_id (getenv (hash-ref BUILD-TYPES service-name)))]
        [repo-token (hasheq 'service_name "cover" 'repo_token repo-token)]
        [else (error "No repo token or ci service detected")]))

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

;; CoverallsCoverage = Nat | json-null

;; Coverage PathString -> [Listof CoverallsCoverage]
;; Get the line coverage for the file to generate a coverage report
(define (line-coverage coverage file)
  (define split-src (string-split (file->string file) "\n"))
  (define file-coverage (hash-ref coverage file))
  (define (process-coverage value rst-of-line)
    (case (covered? value file-coverage file)
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
            [else (define nw-count (+ count (string-length line)))
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
