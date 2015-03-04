CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra
ZADANIA=zadanie1.out zadanie2.out zadanie3.out zadanie4.out zadanie5.out zadanie6.out zadanie7.out zadanie8.out zadanie9.out zadanie10.out
	
zadania: $(ZADANIA)

zadanie10.out: zadanie10.c
	rm -f zadanie10.out
	$(CC) $(CFLAGS) -pthread zadanie10.c -o zadanie10.out

%.out: %.c
	rm -f $@
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(ZADANIA)
