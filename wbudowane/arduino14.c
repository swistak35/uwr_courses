#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define COUNT 1
#define BITS 32
#define DEBUG 1

int liczby[COUNT];

const char *byte_to_binary(int x) {
    static char b[9];
    b[0] = '\0';

    int z;
    for (z = 128; z > 0; z >>= 1) {
      strcat(b, ((x & z) == z) ? "1" : "0");
    }

    return b;
}

void metoda1() {
  int r, mask = 1;
  for (int i = 0; i < COUNT; i++) {
    r = 0;
    printf("%s -> ", byte_to_binary(liczby[i]));
    for (int j = 0; j < BITS; j++) {
      r |= (liczby[i] & mask);
      r <<= 1;
      liczby[i] >>= 1;
    }
    printf("%s\n", byte_to_binary(liczby[i]));
    printf("%d\n", liczby[i]);
  }
}

int main(void) {
  liczby[0] = 1234567890;
  metoda1();

  return 0;
}
