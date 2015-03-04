#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#define OUTBUF_LEN 1000

int main(int argc, char *argv[]) {
  int l, n;

  if (argc == 3) {
    // : )
  } else if (argc == 4) {
    int outbuf_len = atoi(argv[3]);
    /* char outbuf[outbuf_len]; */
    setvbuf(stdout, NULL, _IOFBF, outbuf_len);
  } else {
    exit(EXIT_FAILURE);
  }

  l = atoi(argv[1]) + 2;
  n = atoi(argv[2]);



  char buffer[l];
  for (int i = 0; i < l; i++) {
    buffer[i] = '*';
  }
  buffer[l-2] = '\n';
  buffer[l-1] = '\0';
  char * buffer2 = buffer + l - 1;

  int i = l;
  int j = 0;
  while (j < n) {
    if (i >= l) {
      i = 2;
    }

    printf("%s", buffer2 - i);
    i++;
    j++;
  }

  return 0;
}
