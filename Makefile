all:
	raco setup --check-pkg-deps cover && raco test . && raco cover -b .

debug:
	raco setup cover && raco test . && raco cover -vb .
