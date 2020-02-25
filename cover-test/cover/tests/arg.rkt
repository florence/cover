#lang racket
(require rackunit)
(check-equal? (command-line #:args (a) a) "a")
