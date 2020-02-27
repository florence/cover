PKGS=cover-lib cover-doc cover-test cover cover-benchmarks
all:
	raco setup --check-pkg-deps --pkgs $(PKGS)
	raco test -p $(PKGS)
	raco cover $(EXTRA) -bp $(PKGS)

