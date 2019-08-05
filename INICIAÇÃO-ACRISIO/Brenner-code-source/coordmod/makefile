.f.o:
	g77 -o $*.o -c $<
	chmod a+r $*.o

all:    chbox  chcg  chdelta  chfind  chlims  chmult  chrot  chshift  chtemp  \
	chbox2 chcg2 chdelta2 chfind2 chlims2 chmult2 chrot2 chshift2 chtemp2 \
	defects  getxyz \
	defects2 getxyz2
	echo done

custom: \
	custom.f ../util/util.a
	g77 -o custom \
	custom.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx custom

custom2: \
	custom.f ../util/util.a
	g77 -o custom2 \
	custom.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx custom2

chbox: \
	chbox.f ../util/util.a
	g77 -o chbox \
	chbox.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chbox

chbox2: \
	chbox.f ../util/util.a
	g77 -o chbox2 \
	chbox.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chbox2

chcg: \
	chcg.f ../util/util.a
	g77 -o chcg \
	chcg.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chcg

chcg2: \
	chcg.f ../util/util.a
	g77 -o chcg2 \
	chcg.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chcg2

chdelta: \
	chdelta.f ../util/util.a
	g77 -o chdelta \
	chdelta.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chdelta

chdelta2: \
	chdelta.f ../util/util.a
	g77 -o chdelta2 \
	chdelta.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chdelta2

chfind: \
	chfind.f ../util/util.a
	g77 -o chfind \
	chfind.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chfind

chfind2: \
	chfind.f ../util/util.a
	g77 -o chfind2 \
	chfind.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chfind2

chlims: \
	chlims.f ../util/util.a
	g77 -o chlims \
	chlims.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chlims

chlims2: \
	chlims.f ../util/util.a
	g77 -o chlims2 \
	chlims.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chlims2

chmult: \
	chmult.f ../util/util.a
	g77 -o chmult \
	chmult.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chmult

chmult2: \
	chmult.f ../util/util.a
	g77 -o chmult2 \
	chmult.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chmult2

chrot: \
	chrot.f ../util/util.a
	g77 -o chrot \
	chrot.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chrot

chrot2: \
	chrot.f ../util/util.a
	g77 -o chrot2 \
	chrot.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chrot2

chshift: \
	chshift.f ../util/util.a
	g77 -o chshift \
	chshift.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chshift

chshift2: \
	chshift.f ../util/util.a
	g77 -o chshift2 \
	chshift.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chshift2

chtemp: \
	chtemp.f ../util/util.a ../ranlib/ranlib.a
	g77 -o chtemp \
	chtemp.f ../util/util.a ../ranlib/ranlib.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx chtemp

chtemp2: \
	chtemp.f ../util/util.a ../ranlib/ranlib.a
	g77 -o chtemp2 \
	chtemp.f ../util/util.a ../ranlib/ranlib.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx chtemp2

defects: \
	defects.f ../util/util.a
	g77 -o defects \
	defects.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx defects

defects2: \
	defects.f ../util/util.a
	g77 -o defects2 \
	defects.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx defects2

getxyz: \
	getxyz.f ../util/util.a
	g77 -o getxyz \
	getxyz.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnuw.f
	chmod a+rx getxyz

getxyz2: \
	getxyz.f ../util/util.a
	g77 -o getxyz2 \
	getxyz.f ../util/util.a \
	../lib/lib.f ../lib/argssun.f ../lib/congnu.f
	chmod a+rx getxyz2
