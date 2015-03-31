CC=gcc
CFLAGS=-Wall -W
CFLAGSLIBS=-std=gnu99 -g
	
# %.out: %.c
# 	rm -f $@
# 	$(CC) $(CFLAGS) $(CFLAGSLIBS) $< -o $@

# %.light: %.c
# 	rm -f $@
# 	$(CC) $(CFLAGSLIBS) $< -o $@

%.c: %.h

traceroute.out: traceroute.c sockwrap.c in_cksum.c
	$(CC) $(CFLAGS) $(CFLAGSLIBS) sockwrap.c in_cksum.c traceroute.c -o $@

clean:
	rm -f *.out
	rm -f *.light

all: traceroute.out

run: traceroute.out
	./traceroute.out
