FC = gfortran 
CFLAGS = `sdl2-config --cflags`
LIBS = lib/libsdl2.a `sdl2-config --libs`
INCLUDE = -I$(CURDIR)/fortran-sdl2/

FILES = src/**.f90
OUTPUT =  -o bin/main.out

all:
	rm -f modules/*.mod
	$(FC) $(CFLAGS) $(OUTPUT) $(FILES) $(LIBS) $(INCLUDE)
	mv *.mod modules/

run:
	./bin/main.out

clean:
	rm -f modules/*.mod
	rm -f src/*.mod
	rm -f *.mod
	rm -f bin/main.out