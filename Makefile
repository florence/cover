all:
	raco setup --check-pkg-deps cover && raco test . && raco cover -b .
