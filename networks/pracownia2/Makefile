CC=gcc
CFLAGS=-Wall -W
CFLAGSLIBS=-std=gnu99 -g
	
%.c: %.h

pobieraczka.out: pobieraczka.c sockwrap.c
	$(CC) $(CFLAGS) $(CFLAGSLIBS) sockwrap.c pobieraczka.c -o $@

clean:
	rm -f *.out
	rm -f *.light

distclean: clean

check:
	xxd test.bin > test.bin.hex
	xxd test2.bin > test2.bin.hex
	diff test.bin.hex test2.bin.hex

all: pobieraczka.out

run: pobieraczka.out
	./pobieraczka.out
