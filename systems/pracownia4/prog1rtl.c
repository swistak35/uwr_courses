#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

int main(void) {
  void * handle;

  handle = dlopen("/home/swistak35/Projekty/uwr_courses/systems/pracownia4/libprog1.so", RTLD_LAZY);
  if (!handle) {
    printf("Lib error: %s\n", dlerror());
  }

  void (*print_one)();
  *(void **) (&print_one) = dlsym(handle, "print_one");
  (*print_one)();

  int result;
  int (*square)(int);
  *(void **) (&square) = dlsym(handle, "square");
  result = (*square)(4);
  printf("%d^2 = %d\n", 4, result);

  void (*print_three)(int);
  *(void **) (&print_three) = dlsym(handle, "print_three");
  (*print_three)(4);

  int foo;
  scanf("%d", &foo);

  dlclose(handle);

  return 0;
}
