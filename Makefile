default: main

prelim: charter design interfaces

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
	
