#lang racket/base
(provide vprintf
         logger-init-message
         logger-covered-message
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

;; copied from racket/logging for backwards combatability reasons,
;; and so we can use the internals
(define (with-intercepted-logging/receiver interceptor proc receiver)
  (let* ([t           (receiver-thread receiver interceptor)])
    (begin0
      (proc)
      (thread-send t 'stop)
      (thread-wait t))))


(define (receiver-thread receiver intercept)
  (thread
   (lambda ()
     (define thd-receive
       (wrap-evt (thread-receive-evt)
                 (lambda _ (thread-receive))))
     (let loop ()
       (let ([l (sync receiver thd-receive)])
         (cond [(eq? l 'stop)
                (void)]
               [else
                (intercept l)
                (loop)]))))))
