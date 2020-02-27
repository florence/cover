#lang racket/base
(require racket/runtime-path
         cover/private/shared
         racket/file
         racket/format
         racket/match
         racket/system
         racket/list
         racket/path)
(provide data benchmark-set)
(define benchmark-set
  (hash
   #;'typed-racket
   #;'(package typed-racket-lib typed-racket-test typed-racket-more math-lib)
   'pict
   '(package pict-lib pict-test)
   'racket-benchmarks
   '(collection tests/racket/benchmarks racket racket/base)))
(define-runtime-path data "data")

(module+ main
  (require racket/cmdline)
  (define clean? #f)
  (define times 10)
  (define bin-path #f)
  (define to-run
    (command-line
     #:once-each
     [("-c" "--clean")
      "delete benchmark data for runs"
      (set! clean? #t)]
     [("-t" "--times")
      t "How many runs to perform"
      (define x (string->number t))
      (unless (exact-positive-integer? times)
        (error 'benchmark "bad number of times to run ~a" t))
      (set! times x)]
     [("-b" "--path-to-bin-dir")
      b "path the dir with racket and raco"
      (set! bin-path (simple-form-path b))]

     #:args benchmarks
     (if (empty? benchmarks)
         (hash-keys benchmark-set)
         (map string->symbol benchmarks))))
  (unless (directory-exists? data)
    (make-directory data))
  (for ([x (in-list to-run)])
    (unless (hash-has-key? benchmark-set x)
      (error 'benchmark "unknown benchmark ~a" x)))
  (for ([x (in-list to-run)])
    (define r (hash-ref benchmark-set x))
    (run-benchmark bin-path
                   times
                   (~a x)
                   (first r)
                   (map ~a (rest r))
                   clean?)))
         
(define (run-benchmark b times name type pkgs clean?)
  (let/ec return
    (define dir (build-path data name))
    (when (directory-exists? dir)
      (cond [clean? (delete-directory/files dir)]
            [else
             (log-cover-benchmark-warning
              "found data from previous run, skipping ~a"
              (path->string dir))
             (return)]))
    (make-directory dir)
    (benchmark-tests b times name type pkgs)
    (benchmark-cover b times name type pkgs)))

(define-runtime-path test-path "run-test-benchmark.rkt")
(define (benchmark-tests b times name type pkgs)
  (log-cover-benchmark-info (format "starting test run for ~a" name))
  (benchmark-command
   'test
   name
   times
   (if b (build-path b "racket") "racket")
   (list*
    (path->string test-path)
    (~a type)
    pkgs)))
(define (benchmark-cover b times name type pkgs)
  (log-cover-benchmark-info (format "starting cover run for ~a" name))
  (benchmark-command
   'cover
   name
   times
   (if b (build-path b "raco") "raco")
   (list*
    "cover" "-Q" "-f" "none"
    (case type
      [(package) "-p"]
      [(collection) "-c"])
    pkgs)))

(define (benchmark-command type name times exec cmd)
  (define command-path (if (path? exec) exec (find-executable-path exec)))
  (define file (build-path data name (~a type ".log")))
  (define f* (open-output-file file #:exists 'error))
  (define testing-vars
    (environment-variables-copy (current-environment-variables)))
  (environment-variables-set!
   testing-vars
   #"PLTSTDOUT" #"info@cover-benchmark")
  (environment-variables-set!
   testing-vars
   #"PLTSTDERR" #"error")
  (dynamic-wind
   void
   (lambda ()
     (for ([x (in-range times)])
       (log-cover-benchmark-info (format "starting run for ~a for ~a" x name))
       (match-define
         (list #f #f _ #f get-status)
         (parameterize ([current-environment-variables testing-vars])
           (apply
            process*/ports
            f*
            (current-input-port)
            (current-error-port)
            command-path
            cmd)))
       (get-status 'wait)
       (unless (eq? (get-status 'status) 'done-ok)
         (error 'benchmark
                "The command ~a errored with status ~a"
                (cons command-path cmd)
                (get-status 'exit-code)))))
   (lambda () (close-output-port f*))))
     
