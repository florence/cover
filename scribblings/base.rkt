#lang racket/base

(require
 scribble/eval
 scribble/manual

 (for-label racket/base cover))

(provide
 (all-from-out scribble/eval
               scribble/manual)
 (for-label (all-from-out racket/base cover)))
