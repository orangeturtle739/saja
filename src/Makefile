default: main
main:
	corebuild -pkgs oUnit,async,cryptokit,yojson main.byte
	
test:
	corebuild -pkgs oUnit,str,unix,async,cryptokit,yojson test.byte && ./test.byte

clean:
	ocamlbuild -clean
	
