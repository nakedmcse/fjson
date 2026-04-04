all: fjson test

fjson: fjson.f90 fjson_tokenizer.f90
	gfortran -ffree-line-length-0 -o fjson_tokenizer.o -c fjson_tokenizer.f90
	gfortran -ffree-line-length-0 -o fjson.o -c fjson.f90
	ar rcs fjson.a fjson.o fjson_tokenizer.o
	rm -f *.o

test: fjson.a test.f90
	gfortran -ffree-line-length-0 -o test fjson.a test.f90

clean:
	rm -f *.o
	rm -f *.a
	rm -f *.mod
	rm -f test
