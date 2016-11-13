default: main

prelim: charter design interfaces

charter:
	cd charter; make
design:
	cd design; make
interfaces:
	cd interfaces; make

main:
	cd src; corebuild -pkgs oUnit main.byte
	
test:
	cd src; corebuild -pkgs oUnit,str,unix test.byte && ./test.byte

clean:
	cd src; ocamlbuild -clean
	
