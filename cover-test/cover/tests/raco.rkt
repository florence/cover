#lang racket
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

(require rackunit racket/runtime-path racket/set)

(require/expose
 cover/raco
 (expand-lib
  filter-exts
  expand-directories expand-directory
  get-info-var
  should-omit?
  remove-excluded-paths
  is-excluded-path?
  get-formats))


(test-begin
 (define p (first (expand-lib '("racket/base"))))
 (check-not-false p)
 (check-true (file-exists? p)))


(check-equal? (filter-exts '("a.rkt" "b.rkt" "c/d/e.scrbl" "a/b/c" "a/b.qqq"))
              '("a.rkt" "b.rkt" "c/d/e.scrbl"))


(define extensions #px#"^(.*)\\.(?i:rkt|scm|scrbl|ss)$")
(define-runtime-module-path cover/main cover/main)
(define root (simple-form-path (build-path (resolved-module-path-name cover/main) "..")))
(define private (build-path root "private"))
(define main.rkt (build-path root "main.rkt"))
(define out
  (set "main.rkt"
       "private/contracts.rkt"
       "private/html/html.rkt"
       "private/format-utils.rkt"
       "private/file-utils.rkt"
       "private/shared.rkt"
       "private/raw.rkt"))
(define-check (do-test ->)
  (parameterize ([current-directory root])
    (check-equal? (list->set
                   (map (compose path->string ->relative)
                        (expand-directories (list (path->string main.rkt)
                                                  (-> (path->string private))))))
                  out)))
(do-test ->relative)
(do-test ->absolute)


(define-runtime-module-path dir cover/tests/bfs)
(test-case "directory expansions"
  (define cur (simple-form-path (build-path (resolved-module-path-name dir) ".." "..")))
  (define path (build-path cur "tests" "basic"))
  (parameterize ([current-directory path])
    (check-equal? (list->set (map (compose path->string ->relative)
                                  (flatten (expand-directory (list extensions)))))
                  (set "prog.rkt"
                       "not-run.rkt"
                       "raise.rkt"
                       "empty-ISL.rkt"
                       "empty-id.rkt"
                       "no-expressions.rkt")))
  (parameterize ([current-directory cur])
    (define omit (map ->absolute (get-info-var cur 'test-omit-paths)))
    (define dirs (map ->absolute (filter list? (flatten (expand-directory (list extensions))))))
    (for ([o omit])
      (check-false (member o dirs)
                   (format "~s ~s" o dirs)))))


(check-true (should-omit? "/Test/t.rkt" '("/Test")))
(check-true (should-omit? "/Test/t.rkt" '("/Test/t.rkt")))
(check-true (should-omit? "/Users/florence/playground/cover/tests/error-file.rkt"
                          '("/Users/florence/playground/cover/tests/error-file.rkt")))
(check-false (should-omit? "/Test/t.rkt" '("/OtherDir")))
(check-true (should-omit? "/Users/florence/playground/cover/tests/error-file.rkt"
                          (list extensions)))
(check-false (should-omit? "/Users/florence/playground/cover/tests/error-file.qq"
                           (list extensions)))


(parameterize ([current-directory (build-path "/tests")])
  (check-equal? (remove-excluded-paths
                 (list (list "/tests/tests/x.rkt" #())
                       "/tests/x/tests/x/x.rkt"
                       "/tests/x.rkt")
                 '("tests"))
                (list "/tests/x.rkt")))


(test-case "test path exclusion"
  (parameterize ([current-directory (build-path "/tests")])
    (check-not-false (is-excluded-path? "/tests/tests/x.rkt" '("tests")))
    (check-false (is-excluded-path? "/tests/x.rkt" '("tests")))
    (check-false (is-excluded-path? "/tests/t/x.rkt" '("tests")))))


(test-begin
 ;; we expect that a standard install has "html", "coveralls", and "raw"
 (define h (get-formats))
 (check-true (hash-has-key? h "html"))
 (check-true (hash-has-key? h "raw")))