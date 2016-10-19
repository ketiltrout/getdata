default:
	${MAKE} clean
	cp ../test/*.[co] .
	mv ../test/*.gc* .
	mv ../src/.libs/*.gc* .
	cp ../src/*.[co] .
	gcov *.o | tee gcov.log

build:
	${MAKE} -C .. clean
	( cd .. && ./configure CFLAGS="--coverage" LDFLAGS=-lgcov )
	${MAKE} -C ..
	${MAKE} -C .. check
	${MAKE} default

clean:
	rm -f *.*
