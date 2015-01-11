#lang scribble/doc
@(require "base.rkt")

@title[#:tag "basics"]{Basic Usage of Cover}

The cover library adds the command @exec{raco cover} command to run test coverage. For
every file it is given it will execute that file and its @racket[test] submodule (it if
exists). It will then dump the coverage information into a directory, by default
@filepath{coverage}. By default the coverage information will be generated as html.

The @exec{raco cover} command accepts the following flags:

@itemize[@item{@Flag{c} or @DFlag{coverage}
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

         @item{@Flag{d} or @DFlag{directory}
               --- Specifies the directory output the coverage too.
               defaults to @filepath{coverage}.}
         @item{@Flag{e} or @DFlag{exclude-from-output}
               --- excludes any directories by given name from the coverage report.
               Files in these directories are still run, they are just excluded from the
               outputted coverage. This flag may appear any number of times.}
         @item{@Flag{p} or @DFlag{exclude-pkg-defaults}
               Equivalent to @exec{-e tests -e info.rkt -e scribblings}}
         @item{@Flag{i} or @DFlag{include-extensions}
               --- include the given regular expression in the list of file patterns
                   used when expanding directories, searching for files to cover.}
         @item{@Flag{v} or @DFlag{verbose}
               --- enable verbose logging}
         @item{@Flag{s} or @DFlag{submod}
               --- run the given submodule instead of the test submodule.}
         ]
