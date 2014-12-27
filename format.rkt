#lang racket
(module+ test (require rackunit "main.rkt"))

(define (file->html coverage paths)
  (for/list ([path paths])
    (define file (file->string path))
    (define cover (hash-ref coverage path))
    (define data
      (let loop ([loc 1] [chars (string->list file)] [mode 'none])
        (match chars
          [(list) (mode->end mode)]
          [(cons c r)
           (define (loop* me) (loop (add1 loc) r m))
           (define m (get-mode loc cover))
           (define encoded (encode-char c))
           (if (eq? m mode)
               (cons encoded (loop* mode))
               (append (mode->end mode)
                       (mode->start m)
                       (list encoded)
                       (loop* m)))])))
    (apply string data)))

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

(define (encode-char c) c)

(define covered-mode-start "<span style=\"color:green\">")
(define uncovered-mode-start "<span style=\"color:red\">")
(define (mode->start mode)
  (string->list
   (match mode
     ['none ""]
     [#t covered-mode-start]
     [#f uncovered-mode-start])))

(define mode-end "</span>")
(define (mode->end mode)
  (string->list
   (match mode
     ['none ""]
     [_ mode-end])))

(module+ test
  (define (test file out)
    (test-files! file)
    (check-equal? (first (file->html (get-test-coverage) (list file)))
                  out)
    (clear-coverage!))
  (test "tests/basic/prog.rkt"
        (string-append covered-mode-start
                       (apply string
                              (map encode-char
                                   (string->list (file->string "tests/basic/prog.rkt"))))
                       mode-end)))
