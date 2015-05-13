#include "BurrowsWheelerTransformation.h"

using namespace std;

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
}

void BurrowsWheelerTransformation::initial_sort() {
  std::vector<std::vector<int>> hvec(256, std::vector<int>(0));

  std::vector<int> tmp_ranks;

  for (int i = 0; i < this->length; i++) {
    hvec[this->source[i]].push_back(i);
    this->ranks.push_back(0);
    this->positions.push_back(i);
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
    this->positions[i] = tmp_ranks[i];
  }

  for (int i = 0; i < this->length; i++) {
    std::cout << i << ": " << this->ranks[i] << std::endl;
    this->target[this->ranks[i]] = this->source[i];
  }
}

void BurrowsWheelerTransformation::next_sort() {
  int k = 0;
  int offset = 1; // 2 ^ k

  // first iteration
  std::vector<std::vector<int>> hvec1(256, std::vector<int>(0));

  std::vector<int> tmp_ranks1;

  for (int i = 0; i < this->length; i++) {
    char c = this->source[get_char_idx(this->positions[i] + offset)];
    hvec1[c].push_back(this->positions[i]);
  }

  for (int i = 0; i < 256; i++) {
    /* std::cout << "Znak `" << i << "`: "; */
    for (int v : hvec1[i]) {
      /* std::cout << v << " "; */
      tmp_ranks1.push_back(v);
    }
    /* std::cout << std::endl; */
  }

  for (int i = 0; i < this->length; i++) {
    this->ranks[tmp_ranks1[i]] = i;
    this->positions[i] = tmp_ranks1[i];
  }

  // second iteration
  std::vector<std::vector<int>> hvec2(256, std::vector<int>(0));

  std::vector<int> tmp_ranks2;

  for (int i = 0; i < this->length; i++) {
    char c = this->source[get_char_idx(this->positions[i])];
    hvec2[c].push_back(this->positions[i]);
  }

  for (int i = 0; i < 256; i++) {
    /* std::cout << "Znak `" << i << "`: "; */
    for (int v : hvec2[i]) {
      /* std::cout << v << " "; */
      tmp_ranks2.push_back(v);
    }
    /* std::cout << std::endl; */
  }

  for (int i = 0; i < this->length; i++) {
    this->ranks[tmp_ranks2[i]] = i;
    this->positions[i] = tmp_ranks2[i];
  }

  for (int i = 0; i < this->length; i++) {
    std::cout << i << ": " << this->ranks[i] << std::endl;
    this->target[this->ranks[i]] = this->source[i];
  }
}

int BurrowsWheelerTransformation::get_char_idx(int idx) {
  if (idx >= this->length) {
    return (idx - this->length + 1);
  } else {
    return idx;
  }
}

/*
 * Poczatek:
 * 0    B A B A C A     3
 * 1    A B A B A C     0
 * 2    C A B A B A     5
 * 3    A C A B A B     1
 * 4    B A C A B A     4
 * 5    A B A C A B     2
 *
 * Iteracja 0:
 * 1    A B A B A C     3
 * 3    A C A B A B     5
 * 5    A B A C A B     4
 * 0    B A B A C A     0
 * 4    B A C A B A     1
 * 2    C A B A B A     2
 * 3 0 5 1 4 2
 * 
 * Iteracja 1a:
 * 0    B A B A C A     3
 * 4    B A C A B A     4
 * 2    C A B A B A     5
 * 1    A B A B A C     0
 * 5    A B A C A B     1
 * 3    A C A B A B     2
 * 0 3 2 5 1 4
 *
 * Iteracja 1b:
 * 1    A B A B A C
 * 5    A B A C A B
 * 3    A C A B A B
 * 0    B A B A C A
 * 4    B A C A B A
 * 2    C A B A B A
 * 3 0 5 2 4 1
 *
 */
