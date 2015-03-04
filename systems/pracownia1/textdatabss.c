#include <stdlib.h>
#include <stdio.h>


int var1;
int var2 = 2;
const int var3 = 3;
const int var4;

int fib(int n) {
  if (n == 0) {
    return 1;
  } else if (n == 1) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

int main(void) {
  printf("%d %d %d %d\n", var1, var2, var3, var4);
  printf("%d\n", fib(50));
  return 0;
}
