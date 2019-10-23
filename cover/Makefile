all:
	raco setup --check-pkg-deps --pkgs cover && raco test . && raco cover -b .

debug:
	raco setup cover && raco test . && raco cover -vb .
