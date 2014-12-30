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
  (define %age (get-percentages/file path coverage))
  `(html ()
    (head ()
          (link ([rel "stylesheet"] [type "text/css"] [href ,path-to-css])))
    (body ()
          ,@(%s->xexprs %age)
          (div ([class "code"])
               ,@(file->html coverage path)))))

(define (%s->xexprs %age)
  (for/list ([(type %) %age])
    `(p () ,(~a type ': " " (~r (* 100 %) #:precision 2) "%") (br ()))))

(module+ test
  (define-runtime-path path "../tests/basic/prog.rkt")
  (test-begin
   (define f (path->string (simplify-path path)))
   (test-files! f)
   (check-equal? (make-html-file (hash-ref (get-test-coverage) f) f "main.css")
                 `(html ()
                   (head () (link ([rel "stylesheet"] [type "text/css"] [href "main.css"])))
                   (body ()
                         (p () "expr: 100%" (br ()))
                         (div ([class "code"])
                              ,@(file->html (hash-ref (get-test-coverage) f) f)))))
   (clear-coverage!)))

(define (file->html cover path)
  (define file (file->string path))
  (let loop ([loc 1] [start 1] [left (string-length file)] [mode (covered? 1 cover path)])
    (define (get-xml)
      (mode-xml mode (encode-string (substring file (sub1 start) (sub1 loc)))))
    (case left
      [(0) (list (get-xml))]
      [else
       (define m (covered? loc cover path))
       (define (loop* start) (loop (add1 loc) start (sub1 left) m))
       (if (eq? m mode)
           (loop* start)
           (cons (get-xml)
                 (loop* loc)))])))

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

(define (encode-string s)
  (reverse
   (for/fold ([r null]) ([c s])
     (cons
      (case c
        [(#\space) 'nbsp]
        [(#\newline) '(br ())]
        [else (string c)])
      r))))
(module+ test
  (check-equal? (encode-string " ")
                '(nbsp))
  (check-equal? (encode-string "\n")
                '((br ()))))

(define (mode-xml mode body)
  (define color
    (case mode
      [(yes) "green"]
      [(no) "red"]
      [(missing) "black"]))
  `(span ((style ,(string-append "color:" color))) ,@body))

(module+ test
  (define (test file out)
    (test-files! file)
    (check-equal? (file->html (hash-ref (get-test-coverage) file)
                              file)
                  out)
    (clear-coverage!))
  (define f (path->string (simplify-path path)))
  (test f
        `((span ((style "color:green"))
          ,@(encode-string (file->string f))))))
