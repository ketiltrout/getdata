default:
	${MAKE} clean
	cp ../test/*.[co] .
	cp ../test/*.gc* .
	cp ../src/.libs/*.gc* .
	cp ../src/*.[co] .
	gcov *.o | tee gcov.log

check:
	${MAKE} -C ../src
	${MAKE} -C ../test check
	${MAKE} default

dbuild:
	${MAKE} -C .. clean
	( cd .. && ./configure CFLAGS="-g --coverage" LDFLAGS=-lgcov \
		--disable-bindings --enable-debug )
	${MAKE} check

build:
	${MAKE} -C .. clean
	( cd .. && ./configure CFLAGS="-g --coverage" LDFLAGS=-lgcov \
		--disable-bindings )
	${MAKE} check

clean:
	rm -f *.*
