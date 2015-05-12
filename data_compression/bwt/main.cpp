#include <iostream>
#include "BurrowsWheelerTransformation.h"


int main() {
  char jakistekst[] = "cos sie stalo";
  BurrowsWheelerTransformation * bwt = new BurrowsWheelerTransformation(13);

  char cel[13];
  bwt->transform(jakistekst, cel);
  std::cout << jakistekst << std::endl;
  std::cout << cel << std::endl;
  return 0;
}
