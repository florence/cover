#lang racket/base
(require racket/runtime-path math/statistics racket/list
         racket/file
         plot/pict
         racket/format
         racket/match
         "collect-data.rkt"
         pict
         racket/class racket/draw)

(struct top (compile run))
(struct ag (cpu real gc))
(struct report (avg std))

(module+ main
  (require racket/cmdline)
  (define to-run
    (command-line
     #:args benchmarks
     (if (empty? benchmarks)
         (hash-keys benchmark-set)
         (map string->symbol benchmarks))))
  (for ([x (in-list to-run)])
    (graph (~a x) #:file? #t)))




(define (graph name #:file? [f? #f])
  (define-values (test cover) (report-on name))
  (define plots
    (build-plots (hash 'test test 'cover cover)))
  (define p (apply hb-append (map plot plots)))
  (if f?
      (pict->pdf p (build-path data (~a name ".png")))
      p))

(define (pict->pdf p file)
  (define dc (new pdf-dc%
                  [width (pict-width p)]
                  [height (pict-height p)]
                  [output file]))
  (send dc start-doc "")
  (send dc start-page)
  (draw-pict p dc 0 0)
  (send dc end-page)
  (send dc end-doc))
(define (build-plots x)
  (define skip 2.5)
  (list
   (list
    (discrete-histogram
     #:skip skip
     #:x-min 0
     #:label "test/compile"
     #:color 1
     (for*/list ([ag (in-list (list ag-cpu ag-real ag-gc))])
       (list (object-name ag)
             (report-avg (ag (top-compile (hash-ref x 'test)))))))
    (discrete-histogram
     #:skip skip
     #:x-min 1
     #:label "cover/compile"
     #:color 2
     (for*/list ([ag (in-list (list ag-cpu ag-real ag-gc))])
       (list (object-name ag)
             (report-avg (ag (top-compile (hash-ref x 'cover))))))))
   (list
    (discrete-histogram
     #:skip skip
     #:x-min 0
     #:label "test/run"
     #:color 1
     (for*/list ([ag (in-list (list ag-cpu ag-real ag-gc))])
       (list (object-name ag)
             (report-avg (ag (top-run (hash-ref x 'test)))))))
    (discrete-histogram
     #:skip skip
     #:x-min 1
     #:label "cover/run"
     #:color 2
     (for*/list ([ag (in-list (list ag-cpu ag-real ag-gc))])
       (list (object-name ag)
             (report-avg (ag (top-run (hash-ref x 'cover))))))))
   (list
    (discrete-histogram
     #:skip skip
     #:x-min 0
     #:label "slowdown/compile"
     #:color 1
     (for*/list ([ag (in-list (list ag-cpu ag-real ag-gc))])
       (list (object-name ag)
             (/
              (report-avg (ag (top-compile (hash-ref x 'cover))))
              (report-avg (ag (top-compile (hash-ref x 'test))))))))
    (discrete-histogram
     #:skip skip
     #:x-min 1
     #:label "slowdown/run"
     #:color 2
     (for*/list ([ag (in-list (list ag-cpu ag-real ag-gc))])
       (list (object-name ag)
             (/
              (report-avg (ag (top-run (hash-ref x 'cover))))
              (report-avg (ag (top-run (hash-ref x 'test)))))))))))
     
    
(define (report-on name)
  (define report-loc (build-path data name))
  (define report-test
    (build-report (build-path report-loc "test.log")))
  (define report-cover
    (build-report (build-path report-loc "cover.log")))
  (values report-test report-cover))

(define (build-report path)
  (define table (parse-lines path))
  (top
   (build-ag (hash-ref table 'compile '())) 
   (build-ag (hash-ref table 'run '()))))

(define (build-ag l)
  (define cpu (map first l))
  (define real (map second l))
  (define gc (map third l))
  (ag
   (report (mean cpu) (stddev cpu))
   (report (mean real) (stddev real))
   (report (mean gc) (stddev gc))))

(define (parse-lines file)
  (define lines (file->lines file))
  (for/fold ([h (hash)])
            ([l (in-list lines)])
    (define (err x) (error 'report "bad line ~s ~s" l x))
    (define p (open-input-string l))
    (unless (regexp-match #rx"cover-benchmark:" p)
      (err ""))
    (define type
      (match (regexp-match #rx"[^:]*:" p)
        ['(#" compile:") 'compile]
        ['(#" run:") 'run]
        [x (err x)]))
    (define v (read p))
    (hash-update h
                 type
                 (lambda (x) (cons v x))
                 empty)))
      