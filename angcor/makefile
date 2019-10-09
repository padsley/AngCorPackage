OBJS= mnhlib.o angcor.o fcnang.o angcorm.o plm.o parcor.o parp.o pardec.o c.o \
factor.o rhomm.o gamcor.o sigma.o uk.o rk.o rhokq.o rholsj.o fiscor.o djmk.o \
plot.o
#FFLAGS=  xlf -bnoquiet -qfixed=80 -c -O2 
#FFLAGS=  xlf -c -O2 
#FFLAGS= f77 -c
FFLAGS= gfortran -c

all: angcor

mnhlib.o: mnhlib.f
	$(FFLAGS) mnhlib.f
angcor.o: angcor.f
	$(FFLAGS)  angcor.f
fcnang.o:fcnang.f
	$(FFLAGS) fcnang.f
angcorm.o:angcorm.f ANGCOR.INC
	$(FFLAGS) angcorm.f
plm.o:plm.f ANGCOR.INC
	$(FFLAGS) plm.f
parcor.o:parcor.f ANGCOR.INC
	$(FFLAGS) parcor.f
parp.o:parp.f ANGCOR.INC
	$(FFLAGS) parp.f
pardec.o:pardec.f
	$(FFLAGS) pardec.f
c.o:c.f
	$(FFLAGS) c.f
factor.o:factor.f
	$(FFLAGS) factor.f
rhomm.o:rhomm.f
	$(FFLAGS) rhomm.f
gamcor.o:gamcor.f
	$(FFLAGS) gamcor.f
sigma.o:sigma.f
	$(FFLAGS) sigma.f
uk.o:uk.f
	$(FFLAGS) uk.f 
rk.o:rk.f
	$(FFLAGS) rk.f
rhokq.o:rhokq.f
	$(FFLAGS) rhokq.f
rholsj.o:rholsj.f
	$(FFLAGS) rholsj.f
fiscor.o:fiscor.f
	$(FFLAGS) fiscor.f
djmk.o:djmk.f
	$(FFLAGS) djmk.f
plot.o:plot.f
	$(FFLAGS) plot.f
angcor: $(OBJS)
	gfortran $(OBJS) -o angcor
