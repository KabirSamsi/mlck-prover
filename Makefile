.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect_clean
	dune clean

loc:
	dune clean
	cloc --by-file --include-lang=OCaml .

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
