Dependencies: 
- ocaml 4.14.0+
- menhir 2.1+
- dune 3.4+

For help installing ocaml, refer to the instructions located in the preface of the Cornell CS 3110 textbook, which is linked [here](https://cs3110.github.io/textbook/chapters/preface/install.html).

Following the CS 3110 textbook Chapter 9, we use menhir as our parser generator. The package can be installed with the ocaml package manager opam using the command `opam install menhir` in terminal. Similarly, if not already installed, the build system package dune can be installed using the command `opam install dune`.

To install the system, run `git clone git@github.coecis.cornell.edu:kas499/3110-final-project.git` to clone the github repository.

Build the system using `dune build` or `make`. Run tests using `make test`.
