#lang racket/base
(provide generate-html-coverage)
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
         "../shared.rkt")


;;; Coverage [PathString] -> Void
(define (generate-html-coverage coverage files [d "coverage"])
  (define dir (simplify-path d))
  (define fs (get-files coverage files dir))
  (define asset-path (build-path dir "assets/"))
  (write-files fs)
  (delete-directory/files asset-path #:must-exist? #f)
  (copy-directory/files assets asset-path))


(define (get-files coverage files dir)
  (define pref
    (for/fold ([r (explode-path (current-directory))])
              ([l (in-list files)])
      (take-common-prefix r (explode-path l))))
  (define file-list
    (for/list ([k (in-list files)]
               #:when (absolute-path? k))
      (log-cover-debug "building html coverage for: ~a\n" k)
      (define exploded (explode-path k))
      (define-values (_ dir-list)
        (split-at exploded (length pref)))
      (define coverage-dir-list
        (cons dir (take dir-list (max 0 (sub1 (length dir-list))))))
      (define relative-output-file (path-replace-suffix (last exploded) ".html"))
      (define output-file
        (apply build-path (append coverage-dir-list (list relative-output-file))))
      (define output-dir (apply build-path coverage-dir-list))
      (define assets-path
        (path->string
         (apply build-path
                (append (build-list (sub1 (length coverage-dir-list)) (const ".."))
                        (list "assets/")))))
      (define xexpr (make-html-file coverage k assets-path))
      (list output-file output-dir xexpr)))
  (define file/path-mapping
    (for/hash ([k (in-list files)]
               [p (in-list file-list)])
      (values k
              (path->string
               (find-relative-path dir (first p))))))
  (define index (generate-index coverage files file/path-mapping))
  (cons (list (build-path dir "index.html") dir index)
        file-list))

;; (Listof (list file-path directory-path xexpr)) -> Void
(define (write-files f)
  (for ([l (in-list f)])
    (match-define (list f d e) l)
    (log-cover-debug "writing html coverage: ~s\n" f)
    (make-directory* d)
    (with-output-to-file f
      #:exists 'replace
      (thunk (write-xexpr e)))))


(define-runtime-path assets "assets")
(define (move-support-files! dir)
  (copy-directory/files assets (build-path dir "assets/")))

;; FileCoverage PathString Path -> Xexpr
(define (make-html-file coverage path assets-path)
  (define covered? (curry coverage path))
  (define cover-info (expression-coverage/file path covered?))
  (define-values (covered total) (values (first cover-info) (second cover-info)))
  `(html ()
    (head ()
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [type "text/css"] [href ,(string-append assets-path "main.css")])))
    (body ()
          ,(%s->xexpr (if (= total 0) 1 (/ covered total)))
          (div ([class "code"]) ,(file->html path covered?)))))

(define (%s->xexpr %)
  `(p () ,(~a "expr" ': " " (~r (* 100 %) #:precision 2) "%") (br ())))

(define (file->html path covered?)
  (define lines (file->lines path))
  `(div ([class "lines-wrapper"])
        ,(div:line-numbers (length lines))
        ,(div:file-lines lines covered?)))



;; File Report
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nat -> Xexpr
;; create a div with line numbers in it
(define (div:line-numbers line-count)
  `(div ([class "line-numbers"])
        ,@(for/list ([num (in-range 1 (add1 line-count))])
            (let ([str-num (number->string num)])
              `(div () (a ([href ,(string-append "#" str-num)]) ,str-num))))))

;; [List String] Covered? -> Xexpr
(define (div:file-lines file-lines covered?)
  (define-values (line-divs _1 _2)
    (for/fold ([lines '()] [pos 1] [line-number 1]) ([line (in-list file-lines)])
      (values (cons (div:file-line line pos covered? line-number) lines)
              (add1 (+ pos (string-length line)))
              (add1 line-number))))
  `(div ([class "file-lines"]) ,@(reverse line-divs)))


;; String Nat Covered? -> Xexpr
;; Build a single line into an Xexpr
(define (div:file-line line pos covered? line-number)
  (cond [(zero? (string-length line)) `(br ([id ,(number->string line-number)]))]
        [else
         (define (build-span str type) `(span ([class ,(symbol->string type)]) ,str))
         (define (add-expr cover-type expr cover-exprs)
           (if cover-type
               (cons (build-span expr cover-type) cover-exprs)
               cover-exprs))

         (define-values (xexpr acc/str coverage-type)
           (for/fold ([covered-exp '()] [expr/acc ""] [current-cover-type #f])
                     ([c (in-string line)] [offset (in-naturals)])
             (cond [(equal? c #\space)
                    (define new-expr (cons 'nbsp (add-expr current-cover-type expr/acc covered-exp)))
                    (values new-expr "" #f)]
                   [(equal? current-cover-type (covered? (+ pos offset)))
                    (values covered-exp (string-append expr/acc (string c)) current-cover-type)]
                   [else
                    (define new-expr (add-expr current-cover-type expr/acc covered-exp))
                    (values new-expr (string c) (covered? (+ pos offset)))])))
         `(div ([class "line"] [id ,(number->string line-number)])
               ,@(reverse (add-expr coverage-type acc/str xexpr)))]))

;; Index File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coverage (Listof PathString) (HashoF PathString PathString) -> Xexpr
;; Generate the index html page for the given coverage information
(define (generate-index coverage files file/path-mapping)
  (define expression-coverage (expression-coverage/all coverage files))
  `(html
    (head ()
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [type "text/css"] [href "assets/main.css"]))
          (script ([src "assets/app.js"])))
    (body ()
          (div ([class "report-container"])
               ,(div:total-coverage expression-coverage)
               ,(table:file-reports expression-coverage file/path-mapping)))))

;; [Hash FilePath ExpressionInfo] -> Xexpr
(define (div:total-coverage expr-coverages)
  (define total-coverage-percentage (expression-coverage-percentage/all expr-coverages))
  `(div ([class "total-coverage"])
        ,(string-append "Total Project Coverage: "
                        (~r total-coverage-percentage #:precision 2)
                        "%")))

;; [Hash FilePath ExpressionInfo] -> Xexpr
(define (table:file-reports expr-coverages file/path-mapping)
  `(table ([class "file-list"])
          (thead ()
                 (tr ()
                     (th ([class "file-name"]) "File" ,(file-sorter "file-name"))
                     (th ([class "coverage-percentage"]) "Coverage Percentage" ,(file-sorter "coverage-percentage"))
                     (th ([class "covered-expressions"]) "Covered Expressions" ,(file-sorter "covered-expressions"))
                     (th ([class "uncovered-expressions"]) "Uncovered Expressions" ,(file-sorter "uncovered-expressions"))
                     (th ([class "total-expressions"]) "Total Expressions" ,(file-sorter "total-expressions"))))
          (tbody ()
                 ,@(for/list ([(k expr-info) (in-hash expr-coverages)])
                     (define path (hash-ref file/path-mapping k))
                     (tr:file-report k path expr-info)))))

(define (file-sorter class-name)
  `(div ([class "sort-icon-up"])))

;; PathString PathString ExpressionInfo -> Xexpr
;; create a div that holds a link to the file report and expression
;; coverage information
(define (tr:file-report name path expr-coverage-info)
  (define local-file
    (path->string (find-relative-path (current-directory) (string->path name))))
  (define covered (first expr-coverage-info))
  (define total (second expr-coverage-info))
  (define percentage (* 100 (if (= total 0) 1 (/ covered total))))
  (define styles `([class "file-info"]))
  `(tr ,styles
       (td ([class "file-name"]) (a ([href ,path]) ,local-file))
       (td ([class "coverage-percentage"]) ,(~r percentage #:precision 2))
       (td ([class "covered-expressions"]) ,(~r covered #:precision 2))
       (td ([class "uncovered-expressions"]) ,(~r (- total covered) #:precision 2))
       (td ([class "total-expressions"]) ,(~r total #:precision 2))))

;; Percentage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Percentage is a Real∈[0,100]

;; [Hash FilePath ExpressionInfo] -> Percentage
;; Get the total expression conversion percentage for the whole project
(define (expression-coverage-percentage/all all-expr-info)
  (define total-covered (for/sum ([v (in-list (hash-values all-expr-info))]) (first v)))
  (define total-exprs (for/sum ([v (in-list (hash-values all-expr-info))]) (second v)))
  (* (if (= total-exprs 0) 1 (/ total-covered total-exprs)) 100))


;; Expression Coverage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ExpressionInfo is a (List Nat Nat) where:
;; the first element is the number of covered expressions
;; the second element is the total number of expressions. This will never be 0.

;; Coverage (Listof PathString) -> [Hash FilePath ExpressionInfo]
;; returns a hash that maps file paths to an ExpressionInfo
(define (expression-coverage/all coverage files)
  (for/hash ([file (in-list files)])
    (values file (expression-coverage/file file (curry coverage file)))))

;; FilePath Covered? -> ExpressionInfo
;; Takes a file path and a Covered? and
;; gets the number of expressions covered and the total number of expressions.
(define (expression-coverage/file path covered?)
  (define (is-covered? e)
    ;; we don't need to look at the span because the coverage is expression based
    (define p (syntax-position e))
    (if p
        (covered? p)
        'missing))

  (define e
    (with-module-reading-parameterization
        (thunk (with-input-from-file path
                 (lambda ()
                   (port-count-lines! (current-input-port))
                   (read-syntax))))))

  (define (ret e) (values (e->n e) (a->n e)))
  (define (a->n e)
    (case (is-covered? e)
      [(covered uncovered) 1]
      [else 0]))
  (define (e->n e) (if (eq? (is-covered? e) 'covered) 1 0))

  (define-values (covered total)
    (let recur ([e e])
      (syntax-parse e
        [(v ...)
         (for/fold ([covered (e->n e)] [count (a->n e)])
                   ([v (in-list (stx->list e))])
           (define-values (cov cnt) (recur v))
           (values (+ covered cov)
                   (+ count cnt)))]
        [e:expr (ret #'e)]
        [_ (values 0 0)])))

  (list covered total))
