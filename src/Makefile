default: main
main:
	corebuild -pkgs oUnit,async,cryptokit,yojson,scrypt,ANSITerminal main.byte
	
test:
	corebuild -pkgs oUnit,str,unix,async,cryptokit,yojson,scrypt test.byte && ./test.byte

clean:
	ocamlbuild -clean
amit:
	corebuild -pkgs oUnit,async,yojson broadcast.byte
	
