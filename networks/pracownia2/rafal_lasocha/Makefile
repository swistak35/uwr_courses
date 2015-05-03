CC=gcc
CFLAGS=-Wall -W
CFLAGSLIBS=-std=gnu99 -g
	
%.c: %.h

client-udp.out: client-udp.c sockwrap.c
	$(CC) $(CFLAGS) $(CFLAGSLIBS) sockwrap.c client-udp.c -o $@

clean:
	rm -f *.out

distclean: clean

check:
	xxd test.bin > test.bin.hex
	xxd test2.bin > test2.bin.hex
	diff test.bin.hex test2.bin.hex

all: client-udp.out
