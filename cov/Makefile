CONFIGURE_FLAGS=CFLAGS="-g --coverage" LDFLAGS=-lgcov --disable-bindings \
								--enable-legacy-api

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
	( cd .. && ./configure ${CONFIGURE_FLAGS} --enable-debug )

build:
	${MAKE} -C .. clean
	( cd .. && ./configure ${CONFIGURE_FLAGS} )

clean:
	rm -f *.*
