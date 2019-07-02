FC=g77

all: chuck

chuck: chuck.o dwlibold.o fmn.o
	$(FC) -o chuck chuck.o dwlibold.o fmn.o

chuck.o: chuck.for	
	$(FC) -c -o chuck.o chuck.for

dwlibold.o: dwlibold.for
	$(FC) -c -o dwlibold.o dwlibold.for

fmn.o: fmn.for
	$(FC) -c -o fmn.o fmn.for

clear: clean

clean:
	rm -f chuck *.o
