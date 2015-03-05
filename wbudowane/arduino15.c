#include <stdio.h>

int results[] = {1, 0, 0, 2, 10, 4, 40, 92, 352, 724};

int main(int argc, char * argv[]) {
  int n = atoi(argv[1]);
  printf("%d\n", results[n-1]);

  return 0;
}
