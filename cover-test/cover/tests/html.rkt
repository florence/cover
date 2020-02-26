#lang racket
(require racket/file
         racket/path
         racket/math
         racket/format
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         racket/string
         syntax/modread
         syntax/parse
         syntax/stx
         (only-in xml write-xexpr)
         cover/private/shared)

(require rackunit cover/cover racket/runtime-path racket/set cover/private/file-utils)
(require/expose cover/private/html/html
                (generate-html-coverage get-files write-files make-html-file
                                        file->html div:line-numbers
                                        div:file-line div:file-lines
                                        div:total-coverage
                                        tr:file-report
                                        expression-coverage-percentage/all))
  
(define-runtime-module-path tests/basic/prog.rkt2 cover/tests/basic/prog)
(define tests/basic/prog.rkt (resolved-module-path-name tests/basic/prog.rkt2))
(define-runtime-module-path tests/basic/not-run.rkt2 cover/tests/basic/not-run)
(define tests/basic/not-run.rkt (resolved-module-path-name tests/basic/not-run.rkt2))
(define-runtime-module-path tests/basic/no-expressions.rkt2 cover/tests/basic/no-expressions)
(define tests/basic/no-expressions.rkt (resolved-module-path-name tests/basic/no-expressions.rkt2)) 
(define root (simple-form-path (build-path tests/basic/not-run.rkt ".." ".." "..")))
  
(define (mock-covered? pos)
  (cond [(<= 1 pos 6) 'covered]
        [(= 6 pos) 'missing]
        [else 'uncovered]))


(parameterize ([current-directory root]
               [current-cover-environment (make-cover-environment)])
  (define temp-dir (make-temporary-file "covertmp~a" 'directory))
  (test-files! tests/basic/prog.rkt)
  (define coverage (get-test-coverage))
  (generate-html-coverage coverage (list (->absolute tests/basic/prog.rkt)) temp-dir)
  (check-true (file-exists? (build-path temp-dir "tests/basic/prog.html"))))


(test-begin
 (parameterize ([current-directory root]
                [current-cover-environment (make-cover-environment)])
   (define f (path->string (simplify-path tests/basic/prog.rkt)))
   (define d "coverage")
   (test-files! f)
   (define coverage (get-test-coverage))
   (define files (get-files coverage (list f) d))
   (define (maybe-path->string p)
     (if (string? p) p (path->string p)))
   (check-equal? (list->set (map (compose maybe-path->string first)
                                 files))
                 (set "coverage/index.html"
                      "coverage/tests/basic/prog.html"))
   (check-equal? (list->set (map (compose maybe-path->string second) files))
                 (set "coverage"
                      "coverage/tests/basic"))))

(test-begin
 (define temp-dir (make-temporary-file "covertmp~a" 'directory))
 (define xexpr '(body ()))
 (define dir (build-path temp-dir "x"))
 (define file (build-path dir "y.html"))
 (write-files (list (list file dir xexpr)))
 (check-equal? (file->string file) "<body></body>"))


(test-begin
 (parameterize ([current-cover-environment (make-cover-environment)])
   (define f (path->string (simplify-path tests/basic/prog.rkt)))
   (test-files! f)
   (define cov (get-test-coverage))
   (define covered? (curry cov f))
   (check-equal? (make-html-file cov f "assets/")
                 `(html ()
                        (head ()
                              (meta ([charset "utf-8"]))
                              (link ([rel "stylesheet"] [type "text/css"] [href "assets/main.css"])))
                        (body ()
                              (p () "expr: 100%" (br ()))
                              (div ([class "code"])
                                   ,(file->html f covered?))))))
 (parameterize ([current-cover-environment (make-cover-environment)])
   (define f (path->string (simplify-path tests/basic/no-expressions.rkt)))
   (test-files! f)
   (define cov (get-test-coverage))
   (define covered? (curry cov f))
   (check-equal? (make-html-file cov f "assets/")
                 `(html ()
                        (head ()
                              (meta ([charset "utf-8"]))
                              (link ([rel "stylesheet"] [type "text/css"] [href "assets/main.css"])))
                        (body ()
                              (p () "expr: 100%" (br ()))
                              (div ([class "code"])
                                   ,(file->html f covered?)))))))

(test-begin
 (parameterize ([current-cover-environment (make-cover-environment)])
   (define f (path->string (simplify-path tests/basic/prog.rkt)))
   (test-files! f)
   (define covered? (curry (get-test-coverage) f))
   (define lines (file->lines f))
   (check-equal? (file->html f covered?)
                 `(div ([class "lines-wrapper"])
                       ,(div:line-numbers (length lines))
                       ,(div:file-lines lines covered?)))))
(test-begin
 (parameterize ([current-cover-environment (make-cover-environment)])
   (define f (path->string (simplify-path tests/basic/not-run.rkt)))
   (test-files! f)
   (define covered? (curry (get-test-coverage) f))
   (define lines (file->lines f))
   (check-equal? (file->html f covered?)
                 `(div ([class "lines-wrapper"])
                       ,(div:line-numbers 3)
                       ,(div:file-lines lines covered?)))))
(test-begin
 (parameterize ([current-cover-environment (make-cover-environment)])
   (define f (path->string (simplify-path tests/basic/no-expressions.rkt)))
   (test-files! f)
   (define covered? (curry (get-test-coverage) f))
   (define lines (file->lines f))
   (check-equal? (file->html f covered?)
                 `(div ([class "lines-wrapper"])
                       ,(div:line-numbers 1)
                       ,(div:file-lines lines covered?)))))

(check-equal?
 (div:line-numbers 5)
 `(div ([class "line-numbers"])
       ,@(build-list 5 (λ (n) `(div () (a ([href ,(format "#~a" (add1 n))])
                                          ,(number->string (add1 n))))))))

(test-begin
 (define lines '("hello world" "goodbye"))
 (check-equal? (div:file-lines lines mock-covered?)
               `(div ([class "file-lines"])
                     ,(div:file-line (first lines) 1 mock-covered? 1)
                     ,(div:file-line (second lines) 12 mock-covered? 2))))

(check-equal? (div:file-line "" 1 mock-covered? 999) '(br ([id "999"])))
(check-equal? (div:file-line "hello world" 1 mock-covered? 2)
              '(div ([class "line"] [id "2"]) (span ([class "covered"]) "hello")
                    nbsp
                    (span ([class "uncovered"]) "world")))


(test-begin (check-equal? (div:total-coverage (hash "foo.rkt" (list 0 10)
                                                    "bar.rkt" (list 10 10)))
                          '(div ([class "total-coverage"]) "Total Project Coverage: 50%")))

(test-begin (check-equal? (tr:file-report "foo.rkt" "foo.html" (list 0 1))
                          '(tr ((class "file-info"))
                               (td ([class "file-name"]) (a ((href "foo.html")) "foo.rkt"))
                               (td ([class "coverage-percentage"]) "0")
                               (td ([class "covered-expressions"]) "0")
                               (td ([class "uncovered-expressions"]) "1")
                               (td ([class "total-expressions"]) "1"))))
(test-begin (check-equal? (tr:file-report "foo.rkt" "foo.html" (list 10 10))
                          '(tr ((class "file-info"))
                               (td ([class "file-name"]) (a ((href "foo.html")) "foo.rkt"))
                               (td ([class "coverage-percentage"]) "100")
                               (td ([class "covered-expressions"]) "10")
                               (td ([class "uncovered-expressions"]) "0")
                               (td ([class "total-expressions"]) "10"))))
(test-begin (check-equal? (tr:file-report "foo.rkt" "foo.html" (list 0 0))
                          '(tr ((class "file-info"))
                               (td ([class "file-name"]) (a ((href "foo.html")) "foo.rkt"))
                               (td ([class "coverage-percentage"]) "100")
                               (td ([class "covered-expressions"]) "0")
                               (td ([class "uncovered-expressions"]) "0")
                               (td ([class "total-expressions"]) "0"))))

(test-begin
 (check-equal?
  (expression-coverage-percentage/all (hash "foo.rkt" (list 0 10)
                                            "bar.rkt" (list 10 10)))
  50))
(test-begin
 (check-equal?
  (expression-coverage-percentage/all (hash "foo.rkt" (list 0 0)
                                            "bar.rkt" (list 0 0)))
  100))