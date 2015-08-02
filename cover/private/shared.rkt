#lang racket/base
(provide vprintf
         logger-init-message
         logger-covered-message
         with-logging-to-port
         with-intercepted-logging)

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
  (let* ([orig-logger (current-logger)]
         ;; We use a local logger to avoid getting messages that didn't
         ;; originate from proc. Since it's a child of the original logger,
         ;; the rest of the program still sees the log entries.
         [logger      (make-logger #f orig-logger)]
         [receiver    (apply make-log-receiver logger log-spec)]
         [stop-chan   (make-channel)]
         [t           (receiver-thread receiver stop-chan interceptor)])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))


(define (receiver-thread receiver stop-chan intercept)
  (thread
   (lambda ()
     (define (clear-events)
       (let ([l (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let ([l (sync receiver stop-chan)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))
