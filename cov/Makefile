default:
	${MAKE} clean
	cp ../test/*.[co] .
	cp ../test/*.gc* .
	cp ../src/.libs/*.gc* .
	cp ../src/*.[co] .
	gcov *.o | tee gcov.log

clean:
	rm -f *.*
