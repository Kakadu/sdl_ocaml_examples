all:
	ocamlbuild -use-ocamlfind les1.native les3.native les4.native les5.native les6.native \
    les1gfx.native 

clean:
	rm -fr _build *.byte *.native *.cm[oixt] *.o
