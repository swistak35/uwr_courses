#include <stddef.h>

size_t length(char * s) {
  size_t length = 0;
  while (*(s++)) {
    ++length;
  }
  return length;
}
