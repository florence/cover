#lang racket
(provide generate-html-coverage)
(require (only-in xml write-xexpr) "format-utils.rkt" racket/runtime-path)

(module+ test
  (require rackunit "../cover.rkt" racket/runtime-path))

;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage [d "coverage"])
  (define dir (simplify-path d))
  (make-directory* dir)
  (define file-list
    (for/list ([(k v) coverage])
      (define exploded (explode-path k))
      (define-values (_ dir-list)
        (split-at exploded
                  (length (explode-path (current-directory)))))
      (define coverage-dir-list
        (cons dir (take dir-list (sub1 (length dir-list)))))
      (define relative-output-file (path-replace-suffix (last exploded) ".html"))
      (define output-file
        (apply build-path (append coverage-dir-list (list relative-output-file))))
      (define output-dir (apply build-path coverage-dir-list))
      (define path-to-css
        (path->string
         (apply build-path
                (append (build-list (sub1 (length coverage-dir-list))
                                    (const ".."))
                        (list "main.css")))))
      (make-directory* output-dir)
      (with-output-to-file output-file
        (λ () (write-xexpr (make-html-file (hash-ref coverage k) k path-to-css)))
        #:exists 'replace)
      output-file))
  (build-index! coverage file-list dir)
  (move-support-files! dir))

(define (build-index! coverage file-list dir)
  (define %ages (get-percentages/top coverage))
  (define xexpr
    `(html
      (head ()
            (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
      (body
       ,@(%s->xexprs %ages)
       (div ()
            ,@(for/list ([file file-list])
                (define f (path->string (apply build-path (rest (explode-path file)))))
                `(p () (a ([href ,f]) ,f)))))))
  (with-output-to-file (build-path dir "index.html")
    #:exists 'replace
    (thunk
     (write-xexpr xexpr))))

(define-runtime-path css "main.css")
(define (move-support-files! dir)
  (copy-file css (build-path dir "main.css") #t))

;; FileCoverage PathString PathString -> Xexpr
(define (make-html-file coverage path path-to-css)
  (define covered? (make-covered? coverage path))
  (define %age (get-percentages/file path covered?))
  `(html ()
    (head ()
          (link ([rel "stylesheet"] [type "text/css"] [href ,path-to-css])))
    (body ()
          ,@(%s->xexprs %age)
          (div ([class "code"])
               ,(file->html path covered?)))))

(define (%s->xexprs %age)
  (for/list ([(type %) %age])
    `(p () ,(~a type ': " " (~r (* 100 %) #:precision 2) "%") (br ()))))

(module+ test
  (define-runtime-path path "../tests/basic/prog.rkt")
  (test-begin
   (define f (path->string (simplify-path path)))
   (test-files! f)
   (define cov (hash-ref (get-test-coverage) f))
   (define covered? (make-covered? cov f))
   (check-equal? (make-html-file cov f "main.css")
                 `(html ()
                   (head () (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
                   (body ()
                         (p () "expr: 100%" (br ()))
                         (div ([class "code"])
                              ,(file->html f covered?)))))
   (clear-coverage!)))

(define (file->html path covered?)
  (define file (file->string path))
  (define-values (lines _)
    (for/fold ([ls null] [pos 1])
              ([line (string-split file "\n")])
      (define-values (rline npos)
        (for/fold ([r null] [pos pos])
                  ([c line])
          (values
           (cons (mode-xml (covered? pos)
                           (encode-char c))
                 r)
           (add1 pos))))
      (values
       (cons `(li () ,@(reverse rline)) ls)
       (add1 npos))))
  `(ol ()
    ,@(reverse lines)))

(define (get-mode loc c)
  (define-values (mode _)
    (for/fold ([mode 'none] [last-start 0])
              ([pair c])
      (match pair
        [(list m (srcloc _ _ _ start range))
         (if (and (<= start loc (+ start range))
                  (or (eq? mode 'none)
                      (> start last-start)))
             (values m start)
             (values mode last-start))])))
  mode)

(define (encode-char c)
  (case c
    [(#\space) 'nbsp]
    [else (string c)]))
(module+ test
  (check-equal? (encode-char #\space)
                'nbsp))

(define (mode-xml mode body)
  (define class
    (case mode
      [(yes) "covered"]
      [(no) "uncovered"]
      [(missing) "missing"]))
  `(span ((class ,class)) ,body))

(module+ test
  (define (test file out)
    (test-files! file)
    (define cov (hash-ref (get-test-coverage) file))
    (define covered? (make-covered? cov file))
    (check-equal? (file->html file covered?)
                  out)
    (clear-coverage!))
  (define f (path->string (simplify-path path)))
  (test f
        `(ol ()
          (li ()
              ,@(for/list ([c (first (string-split (file->string f) "\n"))])
                 `(span ((class "covered"))
                   ,(encode-char c))))
          ,@(for/list ([l (rest (string-split (file->string f) "\n"))])
              `(li ()
                ,@(for/list ([c l])
                    `(span ((class ,(if (equal? c #\space) "missing" "covered")))
                      ,(encode-char c))))))))
