#include <stdlib.h>
#include <stdio.h>

int main(int argc, char * argv[]) {
  int x;

  while (scanf("%d\n", &x) > 0) {
    if ((x != 0) && ((x & (x - 1)) != 0)) {
      printf("%d\n", x);
    }
  }
  return 0;
}
