#lang racket/base
(provide logger-init-message
         logger-covered-message
         with-intercepted-logging/receiver
         log-cover-debug
         log-cover-info
         log-cover-warning
         log-cover-error
         log-cover-fatal
         log-cover-benchmark-info
         log-cover-benchmark-warning)

(define logger-init-message "init")
(define logger-covered-message "covered")


(define-logger cover)
(define-logger cover-benchmark)

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
