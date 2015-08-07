#lang racket/base
(provide vprintf
         logger-init-message
         logger-covered-message
         with-logging-to-port
         with-intercepted-logging
         with-intercepted-logging/receiver)

(define logger-init-message "init")
(define logger-covered-message "covered")

;; like printf but only in verbose mode
(define (vprintf #:formatter [format format] . a)
  (log-message (current-logger)
               'debug
               'cover
               (apply format a)
               #f))

;; copied from racket/logging for backwards combatability reasons
(define (with-logging-to-port port proc . log-spec)
  (apply with-intercepted-logging
         (lambda (l) (displayln (vector-ref l 1) ; actual message
                                port))
         proc
         log-spec))

(define (with-intercepted-logging interceptor proc . log-spec)
  (let* ([logger      (make-logger #f (current-logger))]
         [receiver    (apply make-log-receiver logger log-spec)])
    (parameterize ([current-logger logger])
      (with-intercepted-logging/receiver interceptor proc receiver))))

(define (with-intercepted-logging/receiver interceptor proc receiver)
  (let* ([t           (receiver-thread receiver interceptor)])
    (begin0
      (proc)
      (thread-send t 'stop) ; stop the receiver thread
      (thread-wait t))))


(define (receiver-thread receiver intercept)
  (thread
   (lambda ()
     (define thd-receive
       (wrap-evt (thread-receive-evt)
                                (lambda _ (thread-receive))))
     (define (clear-events)
       (let ([l (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let ([l (sync receiver thd-receive)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))
