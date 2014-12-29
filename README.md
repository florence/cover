# Cover

This library is a supplement to rackets `raco test` that adds code coverage capabilities.

## How to install

install via `raco pkg install cover`. To install for development, checkout the repo into a folder named `cover` and in the parent directory run `raco pkg install cover/`.

## How to use

To view the arguments for Cover run `raco cover -h`.

Code coverage can be generated by specifying the `-c <format>` flag.
Right now the valid formats are: `html` and `coveralls`.

`html` simply generates html files for each source file containing coverage information and
highlighted source code. 

`coveralls` generates a coveralls coverage report and sends it to coveralls.
Using coveralls requires the "COVERALLS_REPO_TOKEN" to be set, and needs bash and curl.
Travic-ci/Coveralls support coming soon...

Note that coveralls expect coverage by line. To convert from an expression based coverage to line based coverage we consider any line with any unrun expression to be not run.

The directory that the coverage is outputted to can be specified with the `-d` flag.

If any tests run by `rackunit` fail, Cover will return with exit code `1`. If all tests pass it will return with exit code `0`.


## Internals

Cover also comes with a racket API for running tests and generating coverage reports. Documentation coming soon...
