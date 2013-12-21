all:
	ocamlbuild -use-ocamlfind les1.native les3.native les4.native les5.native

clean:
	rm -fr _build *.byte *.native *.cm[oixt] *.o


