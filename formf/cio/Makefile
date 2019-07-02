#-------------------------------------------------------------------
# $Header$
#
# Makefile for the Fortran CIO library.
#
# $Log$
#-------------------------------------------------------------------

#FFLAGS = -fno-globals -Wno-globals
FC = gfortran

# The default rule to be build if no targets are given on the command line.

default: libcio.a

# All objects

OBJS = 	cio1st.o	\
	cioaif.o	\
	ciobuf.o        \
	ciocmd.o        \
	ciocmx.o        \
	ciodss.o        \
	cioerl.o        \
	ciogbf.o        \
	ciogit.o        \
	ciohlp.o        \
	cioinv.o        \
	ciolit.o        \
	ciolun.o        \
	ciolunpool.o    \
	ciolup.o        \
	ciomod.o        \
	ciomov.o        \
	ciomsg.o        \
	ciopab.o        \
	cioprm.o        \
	cioqco.o        \
	cioqlu.o        \
	cioqpu.o        \
	cioqsh.o	\
	cioqx.o         \
	ciotbe.o        \
	ciowrt.o        \
	ciox.o          \
	cioynl.o        \
	ciostring.o

# All headers

HDRS =  ciocom.inc	\
	cio003ini.inc

# See make doc 4.11: Multiple rules for one target.
# Let all objects depend on all headers. The implicit rule already
# tells that they must be made from their .f files.

$(OBJS):	$(HDRS)

# --- Build all versions

.PHONY: all
objects:	$(OBJS)

libcio.a:	$(OBJS)
	ar rc libcio.a $(OBJS)
#	ar -cr ciolib.a $(OBJS)

# --- Various test progams

test:	tcio1 tcio2 tciolink

tcio1:		tcio1.f ciolib.a
tcio2:		tcio2.f ciolib.a
tciolink:	tciolink.f ciolib.a

# --- Cleanup

clean:
	rm -vf *.o *~ *.bck core

realclean:	clean
	rm -vf libcio.a
