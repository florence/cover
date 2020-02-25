#lang scribble/doc
@(require "base.rkt")

@title[#:tag "plugin"]{Creating Custom Output Formats}

@bold{Warning:} The API presented here is unstable, and may change without warning.

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
               @racket[(->* (coverage/c (listof any/c)) (path-string?) any)], and is the implementation of
               the format. The first argument is the coverage map, the second are the files in the map, and the
               third is the directory to put the coverage in.}
         }

Output formats should log any ``verbose'' output at the @racket['debug] level to the topic
@racket['cover]. These are printed with Cover's @Flag{v} flag.
