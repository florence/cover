#lang racket
(provide generate-html-coverage)
(require (only-in xml write-xexpr) "format-utils.rkt")

(module+ test
  (require rackunit "../cover.rkt" racket/runtime-path))

;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage [dir "coverage"])
  (make-directory* dir)
  (for ([(k v) coverage])
    (define relative-file-name
      (string-replace k (path->string (build-path (current-directory))) ""))
    (define coverage-path (path->string (build-path (current-directory) dir)))
    (define coverage-file-relative
      (string-replace (string-replace relative-file-name ".rkt" "") "/" "-"))
    (define output-file (string-append coverage-path "/" coverage-file-relative ".html"))
    (with-output-to-file output-file
      (λ () (write-xexpr (make-html-file (hash-ref coverage k) relative-file-name)))
      #:exists 'replace)))

;;;;; html
;; FileCoverage PathString -> Xexpr
(define (make-html-file coverage path)
  (define %age (get-percentages/file path coverage))
  `(html ()
    (body ()
          ,@(for/list ([(type %) %age])
              `(p () ,(~a type ': " " (~r (* 100 %) #:precision 2) "%") (br ())))
          ,@(file->html coverage path))))

(module+ test
  (define-runtime-path path "../tests/basic/prog.rkt")
  (test-begin
   (define f (path->string (simplify-path path)))
   (test-files! f)
   (check-equal? (make-html-file (hash-ref (get-test-coverage) f) f)
                 `(html ()
                   (body ()
                         (p () "expr: 100%" (br ()))
                         ,@(file->html (hash-ref (get-test-coverage) f) f))))
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

(define (encode-string c)
  (foldr (λ (el rst) (cons el (cons '(br ()) rst)))
         '()
         (string-split c "\n")))

(define (mode-xml mode body)
  (define color
    (case mode
      [(yes) "green"]
      [(no) "red"]
      [(missing) "black"]))
  `(div ((style ,(string-append "color:" color))) ,@body))

(module+ test
  (define (test file out)
    (test-files! file)
    (check-equal? (file->html (hash-ref (get-test-coverage) file)
                              file)
                  out)
    (clear-coverage!))
  (define f (path->string (simplify-path path)))
  (test f
        `((div ((style "color:green"))
          ,@(encode-string (file->string f))))))
