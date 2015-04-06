#lang scribble/doc
@(require "base.rkt")

@title[#:tag "api"]{Racket API}

@defmodule[cover #:use-sources (cover)]

In addition to being a raco tool, Cover provides racket bindings for running
tests and collecting coverage information. The following are the basic
functions of test coverage.

@section[#:tag "higher"]{A High Level API}


@defproc[(test-files! (#:submod submod symbol? 'test)
                      (files
                        (or/c path-string?
                              (list/c path-string?
                                       (vectorof string? #:immutable #t)))) ...)
                      any]{

Runs all given @racket[files] and their submodule @racket[submod] (if it exists), storing the
coverage information.  If the path is paired with a vector then that vector is used as the
@racket[current-command-line-arguments] when executing that file. This vector must be immutable and
not wrapped by a @racket[chaperone?] or @racket[impersonator?], nor may its elements be wrapped in a
@racket[chaperone?] or @racket[impersonator?]. The function returns false if any tests fail.  Test
coverage information is still collected when test fail.  Test coverage info is added to existing
coverage info.}

@defproc[(clear-coverage! [environment environment? (current-cover-environment)]) any]{
Clears all coverage information.}

@defproc[(get-test-coverage [environment environment? (current-cover-environment)]) coverage/c]{
Gets the current coverage information.}

@defthing[coverage/c
          contract?
          #:value (-> any/c exact-positive-integer?
                       (or/c 'covered 'uncovered 'irrelevant))]{
Coverage information is represented as a mapping from a file's key and a  character
location to whether or not that location in the file was covered.

The files key is determined by what @racket[syntax-source] is on the syntax of the program after
reading it. Typically this is the @racket[string?] for of the absolute path of the file path. If
coverage was run manually, as in the @tech{Lower Lever API}, this value may be something
else.

The character locations are @racket[1] indexed.

There are three possible results for coverage: @itemize[@item{@racket['irrelevant] --- The location
is not considered relevant to coverage information.  It is either not in the coverage information;
is in a submodule specified by @racket[irrelevant-submodules]; is a @racket[begin-for-syntax] form;
or lexes (in the sense of that language's @racket[_color-lexer]) as a comment or whitespace.}
@item{@racket['covered] --- The location is not @racket['irrelevant] and is covered}
@item{@racket['uncovered] --- The location is not @racket['uncovered] and is not covered}] }

@defthing[irrelevant-submodules (parameter/c (or/c #f (listof symbol?)))]{

A parameter that controls with submodules are considered irrelevant by @racket[get-test-coverage]. It
defaults to @racket[#f], which tells @racket[make-covered?] to consider all submodules
irrelevant. If its value is a list, then each element of that list is the name of a submodule to be
considered irrelevant.}

@deftogether[(@defproc[(generate-coveralls-coverage (c coverage/c) (files (listof path-string?))
                                                    (p path-string? "coverage"))
                        any]
              @defproc[(generate-html-coverage (c coverage/c) (files (listof path-string?))
                                               (p path-string? "coverage"))
                        any])]{

Generates coverage information in the coveralls and html formats. Equivalent to the specifications
of the @Flag{c} argument to @exec{raco cover}. Both use @racket[make-covered?] to determine file
coverage.}

@section[#:tag "lower"]{A Lower Level API}

The high level API may not be enough for some applications. For example an IDE may need separate
instances of the coverage table, or may need direct access to the namespace code is run in. For this
purpose @racket[cover] directly expose coverage environments in its @deftech{Lower Lever API}.

Coverage environments are values that package together a coverage namespace, a compiler for
annotating code, and a coverage table to write coverage results to. All other coverage functions use
the @racket[current-cover-environment] for code coverage, unless explicitly given a different
environment.

@defproc[(environment? [v any/c]) any/c]{
Tests if the given value is a coverage environment.}
@defparam[current-cover-environment environment environment?
          #:value (make-cover-environment)]{
The current coverage environment. Defaults to an environment built from
@racket[make-empty-namespace]}
@defproc[(environment-namespace [environment environment?]) namespace?]{
Get the namespace that coverage should be run in. This is the same namespace given to
@racket[make-cover-environment]}
@defproc[(environment-compile [environment environment?])
         (any/c boolean? . -> . compiled-expression?)]{

Returns a value suitable for @racket[current-compile] that will compile code with coverage
annotations.  That code must be run in @racket[environment]'s namespace.}

@defproc[(make-cover-environment [namespace namespace? (make-empty-namespace)]) environment?]{

Makes a coverage environment such that @racket[environment-namespace] will return
@racket[namespace], and @racket[namespace] will be set up to handle coverage information.}
