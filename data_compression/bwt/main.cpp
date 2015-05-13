#include <iostream>
#include "BurrowsWheelerTransformation.h"


int main() {
  char jakistekst[] = "babaca";
  BurrowsWheelerTransformation * bwt = new BurrowsWheelerTransformation(6);

  char cel[6];
  bwt->transform(jakistekst, cel);
  std::cout << jakistekst << std::endl;
  std::cout << cel << std::endl;
  bwt->next_sort();
  std::cout << cel << std::endl;
  return 0;
}
