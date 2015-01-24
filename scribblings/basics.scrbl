#lang scribble/doc
@(require "base.rkt")

@title[#:tag "basics"]{Basic Usage of Cover}

The cover library adds the command @exec{raco cover} command to run test coverage. For
every file it is given it will execute that file and its @racket[test] submodule (it if
exists). It will then dump the coverage information into a directory, by default
@filepath{coverage}. By default the coverage information will be generated as html.

The @exec{raco cover} command accepts the following flags:

@itemize[@item{@Flag{f} or @DFlag{format}
               --- Sets the coverage output type. This flag defaults to html.
               valid formats are:
               @itemize[@item{html: Generates one html file per tested file.}
                        @item{coveralls: generates a coveralls json file.
                              This will then read COVERALLS_REPO_TOKEN from the environment
                              and submit the report to coveralls using that repo token. It should be
                              noted that, for the moment, coveralls requires both bash and curl to
                              run.
                              This is also suitable for pushing to coveralls from Travis CI.}
                        @item{raw: @racket[write]s the raw output @racket[get-test-coverage] to the
                              output file.}]}
         @item{@Flag{b} or @DFlag{exclude-pkg-basics}
              Equivalent to @exec{-n tests -n info.rkt -n scribblings}}
         @item{@Flag{d} or @DFlag{directory}
               --- Specifies the directory to output coverage information to.
               defaults to @filepath{coverage}.}
         @item{@Flag{n} or @DFlag{--no-output-for-path}
               --- excludes any directories by name from the coverage report.
               Files in these directories are still run, they are just excluded from the
               outputted coverage. This flag may appear any number of times.}
         @item{@Flag{i} or @DFlag{include-extensions}
               --- include the given regular expression in the list of file patterns
                   used when expanding directories, searching for files to cover.}
         @item{@Flag{v} or @DFlag{verbose}
               --- enable verbose logging}
         @item{@Flag{s} or @DFlag{submod}
               --- run the given submodule instead of the test submodule.}
         @item{@Flag{c} or @DFlag{collection}
               --- Interprets the arguments as collections whose content should be
               tested (in the same way as directory content).}
         @item{@Flag{p} or @DFlag{package}
               --- Interprets the arguments as packages whose contents should be tested
               (in the same way as directory content). All package scopes are searched
               for the first, most specific
               @tech[#:doc '(lib "pkg/scribblings/pkg.scrbl")]{package scope}.}]


In addition @exec{raco cover} supports the @racket[_test-omit-paths] and
@racket[_test-command-line-arguments] @filepath{info.rkt} options like @exec{raco test}.  In
addition cover supports @racket[_cover-omit-paths], which is identical to @racket[_test-omit-paths],
but is specific to cover.

@section{Gotcha's}

Sometimes the code that cover run will have a non obvious cyclic dependency on another file or
collection. for example, the @racketmodname[net/dns] library has a build time dependency on
@racketmodname[scribble/manual], which depends on @racket[planet], which depends on
@racketmodname[net/dns]. Attempting to run @exec{raco cover} on @racketmodname[net/dns] will result in a
linkage error. This is because Cover recompiled @racketmodname[net/dns] in memory, but @racket[planet]
hasn't been recompiled and thus throws and bad linkage error. There are two ways to fix this. The
first is to include @racketmodname[scribble/manual] and @racket[planet] in the code covered then
exclude them from the output with the @Flag{e} flag. The other is to add the files that cause the
cyclic dependencies to @racket[_test-omit-paths] or @racket[_cover-omit-paths] in that collections
@filepath{info.rkt}.
