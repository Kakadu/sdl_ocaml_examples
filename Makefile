all:
	ocamlbuild -use-ocamlfind main.native les3.native #-verbose 5

clean:
	rm -fr _build *.byte *.native *.cm[oixt] *.o


