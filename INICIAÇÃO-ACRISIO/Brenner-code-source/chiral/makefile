.f.o:
	gfortran -o $*.o -c $<
	chmod a+r $*.o

all: chiral chiral2
	echo "All done."

chiral: \
	chiral.f chiral.a ../util/util.a ../ranlib/ranlib.a
	gfortran -o chiral \
	chiral.f chiral.a ../util/util.a ../ranlib/ranlib.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chiral

chiral2: \
	chiral.f chiral.a ../util/util.a ../ranlib/ranlib.a
	gfortran -o chiral2 \
	chiral.f chiral.a ../util/util.a ../ranlib/ranlib.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chiral2

chiral.a: \
	comdiv.f compat.f factor.f gtstrn.f prifnd.f trans.f
	gfortran -O -c $?
	ar r chiral.a *.o
	ranlib chiral.a
	rm *.o
