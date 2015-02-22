#lang scribble/doc
@(require "base.rkt")

@title[#:tag "api"]{Racket API}

@defmodule[cover #:use-sources (cover)]

In addition to being a raco tool, Cover provides racket bindings for running
tests and collecting coverage information. The following are the basic
functions of test coverage.

@deftogether[(@defthing[coverage/c
                         contract?
                         #:value (hash/c any/c file-coverage/c)]
                @defthing[file-coverage/c contract? #:value (listof (list/c boolean? srcloc?))])]{

Coverage information is a hash map mapping absolute file paths to a list detailing the coverage of
that file. The file is keyed on the @racket[syntax-source] of the syntax objects from that
file. Usually this will be the absolute path to the file. The file coverage information is a list of
lists, mapping a boolean to a range of characters within the file. True means the @racket[srcloc]
structure represents an expression that was run, and False means the structure represents an
expression that was not run. Some expressions may not be represented directly in this coverage
information.  For example, type annotations in @racketmodname[typed/racket] removed during macro
expansion and are thus neither run or not run.  Note that the @racket[srcloc]s are one indexed,
meaning a @racket[1] represents the first character in the file.}

@defproc[(test-files! (#:submod submod symbol? 'test)
                      (files
                        (or/c path-string?
                              (list/c path-string?
                                      (not-impersonated/c
                                       (vectorof (not-impersonated/c string?) #:immutable #t))))) ...)
                      any]{

Runs all given @racket[files] and their submodule @racket[submod] (if it exists), storing the
coverage information.  If the path is paired with a vector then that vector is used as the
@racket[current-command-line-arguments] when executing that file. This vector must be immutable and
not wrapped by a @racket[chaperone] or @racket[impersonator], nor may its elements be wrapped in a
chaperone or impersonator. The function returns false if any tests fail.  Test coverage information
is still collected when test fail.  Test coverage info is added to existing coverage info.}

@defproc[(clear-coverage! [environment environment? (current-coverage-environment)]) any]{
Clears all coverage information.}

@defproc[(get-test-coverage [environment environment? (current-coverage-environment)]) coverage/c]{
Gets the current coverage information.}
@defproc[(make-covered? (coverage file-coverage/c) (path path-string?))
         (->* (exact-positive-integer?)
              (#:byte? boolean?)
              (or/c 'covered 'uncovered 'irrelevant))
         ]{
Given some location in a file and the
coverage information for that file @racket[make-covered?] returns
a functions that determines if some @racket[1] indexed character or byte location
in that file is covered. By default it checks character locations.

There are three possible results: @itemize[@item{@racket['irrelevant] --- The location is not
considered relevant to coverage information.  It is either not in the coverage information; is in a
submodule specified by @racket[irrelevant-submodules]; is a @racket[begin-for-syntax] form; or lexes
(in the sense of that languages, @racket[_color-lexer]) as a comment or whitespace.}
@item{@racket['covered] --- The location is not @racket['irrelevant] and is covered}
@item{@racket['uncovered] --- The location is not @racket['uncovered] and is not covered}] }

@defthing[irrelevant-submodules (parameter/c (or/c #f (listof symbol?)))]{

A parameter that controls with submodules are considered irrelevant by @racket[make-covered?]. It
defaults to @racket[#f], which tells @racket[make-covered?] to consider all submodules
irrelevant. If its value is a list, then each element of that list is the name of a submodule to be
considered irrelevant.}

@defproc[(environment? [v any/c]) any/c]{
Tests if the given value is a coverage environment.}
@defthing[current-coverage-environment (parameter/c environment?)]{
The current coverage environment}
@defproc[(environment-namespace [environment environment?]) namespace?]{
Get the namespace that coverage should be run in}
@defproc[(environment-compile [environment environment?])
         (any/c boolean? . -> . compiled-expression?)]{

Returns a value suitable for @racket[current-compile] that will compile code with coverage
annotations.  That code must be run in @racket[environment]'s namespace.}

@defproc[(make-cover-environment [namespace namespace? (make-base-namespace)]) environment?]{

Makes a coverage environment such that @racket[environment-namespace] will return
@racket[namespace], and @racket[namespace] will be set up to handle coverage information.}

@deftogether[(@defproc[(generate-coveralls-coverage (c coverage/c) (p path-string? "coverage")) any]
              @defproc[(generate-html-coverage (c coverage/c) (p path-string? "coverage")) any])]{

Generates coverage information in the coveralls and html formats. Equivalent to the specifications
of the @Flag{c} argument to @exec{raco cover}. Both use @racket[make-covered?] to determine file
coverage.}
