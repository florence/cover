#lang scribble/doc
@(require "base.rkt")

@title[#:tag "plugin"]{Creating Custom Output Formats}

Any package may add an output format to cover. A format is, roughly, a function that takes
coverage information and transforms it into some other format. To add a format
put a definition for @racket[cover-formats] into a packages @filepath["info.rkt"]. This should be a
@racket[(listof (list _command-name _module-path _function-name))]:
@itemize{
         @item{@racket[_command-name] is a string which will be used as the argument to @exec{-c} to
               use this format.}
         @item{@racket[_module-path] should be the path to a racket file providing this format.}
         @item{@@racket[_function-name] should be a symbol that is
               bound to a function in @racket[_module-path]. It should match the contract
               @racket[(->* (coverage/c) (path-string?) any)], and is the implementation of
               the format.}
         }
