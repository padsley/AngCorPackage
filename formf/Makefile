FC=gfortran
LIBS=cio/

all: formf

formf: formf.o mnhlib.o bellib.o 
	$(FC) -g -o formf formf.o mnhlib.o bellib.o -L$(LIBS) -lcio

formf.o: formf.for	
	$(FC) -g -c -o formf.o formf.for

mnhlib.o: mnhlib.for
	$(FC) -g -c -o mnhlib.o mnhlib.for

bellib.o: bellib.for
	$(FC) -g -c -o bellib.o bellib.for


clean:
	rm -f formf *.o
