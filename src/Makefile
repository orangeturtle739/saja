default: main
main:
	corebuild -pkgs oUnit,async,cryptokit,yojson,scrypt,ANSITerminal saja.byte

test:
	corebuild -pkgs oUnit,str,unix,async,cryptokit,yojson,scrypt test.byte && ./test.byte

clean:
	ocamlbuild -clean
amit:
	corebuild -pkgs oUnit,async,yojson broadcast.byte
tcp:
	corebuild -pkgs core,async msgtransport.byte
discovery:
	corebuild -pkgs core,async,cryptokit,scrypt discovery.byte
logo:
	./logo.sh
