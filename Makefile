all: clean output

output:
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native fouine

zinc : clean
	ocamlbuild -yaccflag -v -lib unix zinc_main.native; ln -fs zinc_main.native zinc

clean:
	ocamlbuild -clean

mrproper: clean
	rm -f fouine
	rm -f zinc
