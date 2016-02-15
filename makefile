all: mapfold expression

mapfold: mapfold.ml
	ocamlbuild mapfold.byte

expression: expression.ml
	ocamlbuild expression.byte

clean:
	rm -rf _build *.byte
