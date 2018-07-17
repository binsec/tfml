all:
	ocamlbuild -use-ocamlfind -I src main.native

clean:
	ocamlbuild -clean
