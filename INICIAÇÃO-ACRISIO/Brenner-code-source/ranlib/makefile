ranlib.a: ranlib.f/src/*.f ranlib.f/linpack/*.f
	g77 -C -Wall -c $?
	ar r ranlib.a *.o
	ranlib ranlib.a
	rm *.o
