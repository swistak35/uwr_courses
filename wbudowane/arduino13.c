#include <stdio.h>
#include <stdlib.h>

#define DEBUG 0

void space_move(char * str, int text_size) {
  char tmp;
  int spaces = 0;
  int i = text_size - 1;
  while (i >= spaces) {

    if (str[i] == ' ') {
      while (i > spaces && str[i-spaces] == ' ') {
        spaces++;
      }

      if (DEBUG) { printf("Bede zamienial `%c` (%d) z `%c` (%d)\n", str[i], i, str[i-spaces], i-spaces); }
      if (DEBUG) { printf("'%s'\n", str); }

      tmp = str[i-spaces];
      str[i-spaces] = str[i];
      str[i] = tmp;
    }
    i--;
  }
}

int main(void) {
  char test1[] = " foo bar baz ";
  char test2[] = "foo bar baz";
  char test3[] = "  foo  bar    baz ";
  char test4[] = " foobarbaz";
  char test5[] = "foobarbaz ";
  char test6[] = "foobarbaz      ";
  char test7[] = "       foobarbaz";

  printf("test1: '%s'\n", test1);
  printf("test2: '%s'\n", test2);
  printf("test3: '%s'\n", test3);
  printf("test4: '%s'\n", test4);
  printf("test5: '%s'\n", test5);
  printf("test6: '%s'\n", test6);
  printf("test7: '%s'\n", test7);

  space_move(test1, sizeof(test1));
  space_move(test2, sizeof(test2));
  space_move(test3, sizeof(test3));
  space_move(test4, sizeof(test4));
  space_move(test5, sizeof(test5));
  space_move(test6, sizeof(test6));
  space_move(test7, sizeof(test7));

  printf("test1: '%s'\n", test1);
  printf("test2: '%s'\n", test2);
  printf("test3: '%s'\n", test3);
  printf("test4: '%s'\n", test4);
  printf("test5: '%s'\n", test5);
  printf("test6: '%s'\n", test6);
  printf("test7: '%s'\n", test7);

  return 0;
}
