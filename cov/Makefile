default:
	${MAKE} clean
	cp ../test/*.[co] .
	mv ../test/*.gc* .
	mv ../src/.libs/*.gc* .
	cp ../src/*.[co] .
	gcov *.o | tee gcov.log

clean:
	rm -f *.*
