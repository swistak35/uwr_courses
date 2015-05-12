#include "BurrowsWheelerTransformation.h"

BurrowsWheelerTransformation::BurrowsWheelerTransformation(int length) {
  this->length = length;
}

BurrowsWheelerTransformation::~BurrowsWheelerTransformation() {
}

int BurrowsWheelerTransformation::transform(char * source, char * target) {
  char * source_end = source + (this->length - 1);
  this->source = source;
  this->target = target;

  initial_sort();

  /* int i = 1; */
  /* while (power(2,i) < this->length) { */
  /* } */
}

void BurrowsWheelerTransformation::initial_sort() {
  std::vector<std::vector<int>> hvec(256, std::vector<int>(0));

  std::vector<int> tmp_ranks;

  for (int i = 0; i < this->length; i++) {
    hvec[this->source[i]].push_back(i);
    this->ranks.push_back(0);
  }

  for (int i = 0; i < 256; i++) {
    /* std::cout << "Znak `" << i << "`: "; */
    for (int v : hvec[i]) {
      /* std::cout << v << " "; */
      tmp_ranks.push_back(v);
    }
    /* std::cout << std::endl; */
  }

  for (int i = 0; i < this->length; i++) {
      this->ranks[tmp_ranks[i]] = i;
  }

  for (int i = 0; i < this->length; i++) {
    /* std::cout << i << ": " << this->ranks[i] << std::endl; */
    this->target[this->ranks[i]] = this->source[i];
  }
}
