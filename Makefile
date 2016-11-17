default: main

prelim: charter design interfaces

charter:
	cd charter; make
design:
	cd design; make
interfaces:
	cd interfaces; make

main:
	cd src; corebuild -pkgs oUnit,async,cryptokit,yojson main.byte
	
test:
	cd src; corebuild -pkgs oUnit,str,unix,async,cryptokit,yojson test.byte && ./test.byte

clean:
	cd src; ocamlbuild -clean
	
