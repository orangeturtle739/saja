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
run: default
	cd src; ./saja.sh
debug: default
	cd src; ./saja.byte
design-clean:
	cd design; make clean
submit: clean default design-clean design
	rm -rf submission
	mkdir submission
	zip -r submission/saja.zip . -x *.git* -x submission* -x src/_build*
	cp design/design.pdf submission/
	git log --show-signature > submission/vclog.txt
	cd submission; md5sum saja.zip vclog.txt design.pdf > sums.txt
	cd submission; cat sums.txt
