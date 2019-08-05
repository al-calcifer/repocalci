util.a: coordg.f coordr.f coordw.f find.f sett.f xyzw.f
	g77 -O -c $?
	ar r util.a *.o
	ranlib util.a
	rm *.o
