CC=gcc
CFLAGS=-Wall -W
CFLAGSLIBS=-std=gnu99 -g
	
%.c: %.h

server.out: server.c sockwrap.c
	$(CC) $(CFLAGS) $(CFLAGSLIBS) sockwrap.c server.c -o $@

clean:
	rm -f *.out
	rm -f *.light

all: server.out

run: server.out
	./server.out 8888 "dir1/strony_www/"
