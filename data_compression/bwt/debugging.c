#include <stdio.h>

unsigned char data[] = { 5, 1, 0, 0, 148, 33, 106, 13, 109, 117, 123, 115, 114, 6, 1, 6, 7, 119, 118, 7, 106, 110, 112 };

int main(void) {
  FILE * source = fopen("testowy_input", "wb");
  fwrite(data, sizeof(data), 1, source);
  fclose(source);
}
