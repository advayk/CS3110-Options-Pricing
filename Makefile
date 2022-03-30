.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

app:
	OCAMLRUNPARAM=b dune exec bin/main.exe

reader:
	OCAMLRUNPARAM=b dune exec bin/csv_loader.exe


check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f enigma.zip
	zip -r enigma.zip . -x@exclude.lst

clean:
	dune clean
	rm -f enigma.zip

doc:
	dune build @doc