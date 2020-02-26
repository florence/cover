#lang racket
(require racket/file
         racket/function
         racket/list
         racket/match
         racket/port
         racket/set
         racket/bool

         syntax-color/racket-lexer
         syntax-color/lexer-contract
         syntax/modread
         syntax/parse

         data/interval-map

         cover/private/shared
         cover/private/format-utils)

(require rackunit racket/runtime-path racket/set)



(require cover/cover)
  
(define-runtime-module-path cover cover/cover)
(define cover.rkt (resolved-module-path-name cover))
(define current-cover-environment
  (dynamic-require cover.rkt 'current-cover-environment))
(define-runtime-module-path path2* cover/tests/prog)
(define path2 (resolved-module-path-name path2*))
(parameterize ([irrelevant-submodules #f])
  (test-begin
   (parameterize ([current-cover-environment (make-cover-environment)])
     (define f (path->string (simplify-path path2)))
     (test-files! f)
     (define coverage (get-test-coverage))
     (define covered? (curry coverage f))
     (check-equal? (covered? 14) 'irrelevant)
     (check-equal? (covered? 17) 'irrelevant)
     (check-equal? (covered? 28) 'irrelevant)
     (check-equal? (covered? 35) 'covered)
     (check-equal? (covered? 52) 'irrelevant)
     (check-equal? (covered? 53) 'irrelevant)
     (check-equal? (covered? 54) 'irrelevant)
     (check-equal? (covered? 50) 'uncovered)
     (check-equal? (covered? 78) 'uncovered)
     (check-equal? (covered? 106) 'uncovered))))