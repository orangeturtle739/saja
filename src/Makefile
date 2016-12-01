default: main
main:
	corebuild -pkgs oUnit,async,cryptokit,yojson,scrypt,ANSITerminal saja.byte

test:
	corebuild -pkgs oUnit,str,unix,async,cryptokit,yojson,scrypt test.byte && ./test.byte

clean:
	ocamlbuild -clean
udp:
	corebuild -pkgs oUnit,async,yojson broadcast_test.byte
tcp:
	corebuild -pkgs core,async msgtransport_test.byte
discovery:
	corebuild -pkgs core,async,cryptokit,scrypt discovery_test.byte
logo:
	./logo.sh
format:
	find . -name '*.ml' ! -name 'logo.ml' -exec ocp-indent -i {} \; -o -name '*.mli' -exec ocp-indent -i {} \;
