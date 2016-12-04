default: main

prelim: charter design interfaces

install:
	cat src/packages.txt | tr '\n' ' ' | xargs opam install

charter:
	cd charter; make
design:
	cd design; make
interfaces:
	cd interfaces; make

main:
	cd src; make

test:
	cd src; make test

clean:
	cd src; make clean
run:
	cd src; ./saja.sh
debug:
	cd src; ./saja.byte
