#lang racket
(provide generate-coveralls-coverage)
(require racket/runtime-path json "format-utils.rkt")

(module+ test
  (require rackunit "../main.rkt" racket/runtime-path))

;; Coveralls

;; Coverage [Hasheq String String] [path-string] -> Void
(define-runtime-path post "curl.sh")
(define (generate-coveralls-coverage coverage meta [dir "coverage"])
  (make-directory* dir)
  (define coverage-path (path->string (build-path (current-directory) dir)))
  (define coverage-file (string-append coverage-path "/coverage.json"))
  (define json (generate-coveralls-json coverage meta))
  (define token (or (getenv "COVERALLS_REPO_TOKEN") ""))
  (with-output-to-file coverage-file
    (λ () (write-json (hash-set (hash-set json 'repo_token token)
                                'service_name
                                "better-test")))
    #:exists 'replace)
  (system* (path->string post) coverage-file))

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
