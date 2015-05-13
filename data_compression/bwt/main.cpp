#include <iostream>
#include "BurrowsWheelerTransformation.h"


int main() {
  char jakistekst[] = "babaca";
  BurrowsWheelerTransformation * bwt = new BurrowsWheelerTransformation(6);

  char cel[6];
  std::cout << jakistekst << std::endl;
  bwt->transform(jakistekst, cel);
  /* std::cout << cel << std::endl; */
  /* bwt->next_sort(); */
  /* std::cout << cel << std::endl; */
  return 0;
}
