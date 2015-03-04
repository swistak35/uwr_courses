#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  if (argc != 3) {
    exit(EXIT_FAILURE);
  }
  int l = atoi(argv[1]) + 2;
  int n = atoi(argv[2]);

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

    write(STDOUT_FILENO, buffer2 - i, i);
    i++;
    j++;
  }

  return 0;
}
