#include <stdlib.h>
#include <unistd.h>
#include <sys/uio.h>
#include <stdio.h>

#define BUFS_MAX 1024

int main(int argc, char *argv[]) {
  int l,n;

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

  struct iovec iovs[BUFS_MAX];

  int i = l;
  int bufs = 0;
  int j = 0;
  while (j < n) {
    if (bufs == BUFS_MAX) {
      writev(STDOUT_FILENO, iovs, bufs);
      bufs = 0;
    }

    if (i >= l) {
      i = 2;
    }

    iovs[bufs].iov_base = (void *) buffer2 - i;
    iovs[bufs].iov_len = i;

    bufs++;
    i++;
    j++;
  }

  writev(STDOUT_FILENO, iovs, bufs);

  return 0;
}
