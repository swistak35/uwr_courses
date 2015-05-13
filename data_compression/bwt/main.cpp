#include <iostream>
#include "LexiBWT.h"
#include "LexiDeBWT.h"

int main() {
  char jakistekst[] = "babacabc";
  LexiBWT * bwt = new LexiBWT(8);

  char cel[9];
  cel[8] = 0;
  std::cout << jakistekst << std::endl;
  int orig_string = bwt->transform(jakistekst, cel);
  std::cout << cel << std::endl;
  std::cout << "Index of orig string: " << orig_string << std::endl;
  return 0;
}
