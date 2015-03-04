#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

void print_one();
int square(int x);
void print_three(int y);

int main(void) {
  print_one();

  int result;
  result = square(4);
  printf("%d^2 = %d\n", 4, result);

  print_three(4);

  int foo;
  scanf("%d", &foo);

  return 0;
}
