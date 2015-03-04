CC=gcc
CFLAGS=-Wall -Wextra -Werror
CFLAGSLIBS=-std=gnu99
ZADANIA=prog2.out prog3.out prog4.out prog5.out prog7.out prog9.out prog6stdio.out prog6write.out


prog1obj1.o: prog1obj1.c
	gcc -std=gnu99 -fPIC -c $<

prog1obj2.o: prog1obj2.c
	gcc -std=gnu99 -fPIC -c $<

prog1obj3.o: prog1obj3.c
	gcc -std=gnu99 -fPIC -c $<

libprog1.so: prog1obj1.o prog1obj2.o prog1obj3.o
	gcc -shared -o $@ prog1obj1.o prog1obj2.o prog1obj3.o

prog1rtl.out: prog1rtl.c libprog1.so
	gcc -ldl $(CFLAGS) $(CFLAGSLIBS) $< -o $@
	
# export LD_LIBRARY_PATH=/home/swistak35/Projekty/uwr_courses/systems/pracownia4
prog1ltl.out: prog1ltl.c libprog1.so
	gcc $(CFLAGS) $(CFLAGSLIBS) -L. -lprog1 $< -o $@

%.out: %.c
	rm -f $@
	$(CC) $(CFLAGS) $(CFLAGSLIBS) $< -o $@

%.light: %.c
	rm -f $@
	$(CC) $(CFLAGSLIBS) $< -o $@

clean:
	rm -f *.out
	rm -f *.light
	rm -f *.o
	rm -f *.so

all: $(ZADANIA)
