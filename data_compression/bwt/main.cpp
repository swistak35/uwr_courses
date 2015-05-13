#include <iostream>
#include "BurrowsWheelerTransformation.h"


int main() {
  char jakistekst[] = "babacabc";
  BurrowsWheelerTransformation * bwt = new BurrowsWheelerTransformation(8);

  char cel[9];
  cel[8] = 0;
  std::cout << jakistekst << std::endl;
  bwt->transform(jakistekst, cel);
  /* std::cout << cel << std::endl; */
  /* bwt->next_sort(); */
  /* std::cout << cel << std::endl; */
  return 0;
}
