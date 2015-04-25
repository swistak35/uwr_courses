CC=gcc
CFLAGS=-Wall -W
CFLAGSLIBS=-std=gnu99 -g
	
%.c: %.h

pobieraczka.out: pobieraczka.c sockwrap.c
	$(CC) $(CFLAGS) $(CFLAGSLIBS) sockwrap.c pobieraczka.c -o $@

clean:
	rm -f *.out
	rm -f *.light

all: pobieraczka.out

run: pobieraczka.out
	./pobieraczka.out
