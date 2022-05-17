.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

pricing:
	OCAMLRUNPARAM=b dune exec bin/main.exe

binomial:
	OCAMLRUNPARAM=b dune exec bin/binomial_loader.exe

reader:
	OCAMLRUNPARAM=b dune exec bin/csv_loader.exe

visualize:
	OCAMLRUNPARAM=b dune exec bin/visualize.exe

interface:
	OCAMLRUNPARAM=b dune exec bin/interface.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f newestrepo.zip
	zip -r newestrepo.zip . -x@exclude.lst

clean:
	dune clean
	rm -f enigma.zip

doc:
	dune build @doc