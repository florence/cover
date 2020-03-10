PKGS=cover-lib cover-doc cover-test cover
all:
	PLTSTDERR="warning" raco setup --check-pkg-deps --pkgs $(PKGS)
	PLTSTDERR="warning" raco test -ep $(PKGS)
	PLTSTDERR="warning" raco cover $(EXTRA) -bp $(PKGS)

