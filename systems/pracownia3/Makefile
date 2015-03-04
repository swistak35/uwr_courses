CC=gcc
CFLAGS=-Wall -Wextra -Werror
CFLAGSLIBS=-std=gnu99 -g -pthread -lrt
ZADANIA=prog2.out prog3.out prog4.out prog5.out prog6.out prog7.out prog9.out
	
%.out: %.c
	rm -f $@
	$(CC) $(CFLAGS) $(CFLAGSLIBS) $< -o $@

%.light: %.c
	rm -f $@
	$(CC) $(CFLAGSLIBS) $< -o $@

clean:
	rm -f *.out
	rm -f *.light

all: $(ZADANIA)
