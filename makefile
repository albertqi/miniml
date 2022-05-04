all: evaluation_tests evaluation expr_tests expr miniml

evaluation_tests: evaluation_tests.ml
	ocamlbuild -use-ocamlfind evaluation_tests.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr_tests: expr_tests.ml
	ocamlbuild -use-ocamlfind expr_tests.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte