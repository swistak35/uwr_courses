#include "BurrowsWheelerTransformation.h"

using namespace std;

BurrowsWheelerTransformation::BurrowsWheelerTransformation(int length) {
  this->length = length;
}

BurrowsWheelerTransformation::~BurrowsWheelerTransformation() {
}

int BurrowsWheelerTransformation::transform(char * source, char * target) {
  this->source_end = source + (this->length);
  this->source = source;
  this->target = target;
  this->hvec.assign(256, std::vector<int>(0));

  initial_sort();
  return 0;
}

void BurrowsWheelerTransformation::initial_sort() {
  // Init vectors
  for (int i = 0; i < this->length; i++) {
    this->ranks.push_back(0);
    this->positions.push_back(i);
  }

  // Initial sort
  for (int i = 0; i < this->length; i++) {
    this->hvec[this->source[i]].push_back(i);
  }

  for (int i = 0; i < 256; i++) {
    /* std::cout << "Znak `" << i << "`: "; */
    for (int v : this->hvec[i]) {
      /* std::cout << v << " "; */
      this->tmp_ranks.push_back(v);
    }
    /* std::cout << std::endl; */
  }

  for (int i = 0; i < this->length; i++) {
    this->ranks[this->tmp_ranks[i]] = i;
    this->positions[i] = this->tmp_ranks[i];
  }

  for (int i = 0; i < this->length; i++) {
    std::cout << " " << this->ranks[i];
    this->target[this->ranks[i]] = this->source[i];
  }
  cout << "\n";
  cout << this->target << endl;

  for (auto &h : this->hvec) {
    h.clear();
  }
  this->tmp_ranks.clear();

  for (int i = 0; i < this->length; i++) {
    display_string(this->positions[i]);
  }
  int k = 0;
  int last_k = log2(this->length) - 1;
  cout << "Ostatnie k: " << last_k << endl;

  while (k <= last_k) {
    int offset = pow(2, k); // 2 ^ k
    cout << "Offset: " << offset << endl;

    sort_iteration_step(offset);
    sort_iteration_step(0);

    /* std::cout << "------" << endl; */

    for (int i = 0; i < this->length; i++) {
      display_string(this->positions[i]);
    }
    k++;
  }
}

void BurrowsWheelerTransformation::sort_iteration_step(int offset) {
  // first iteration

  for (int i = 0; i < this->length; i++) {
    char c = this->source[get_char_idx(this->positions[i] + offset)];
    this->hvec[c].push_back(this->positions[i]);
  }

  for (int i = 0; i < 256; i++) {
    /* std::cout << "Znak `" << i << "`: "; */
    for (int v : this->hvec[i]) {
      /* std::cout << v << " "; */
      this->tmp_ranks.push_back(v);
    }
    /* std::cout << std::endl; */
  }

  for (int i = 0; i < this->length; i++) {
    this->ranks[this->tmp_ranks[i]] = i;
    this->positions[i] = this->tmp_ranks[i];
  }

  for (int i = 0; i < this->length; i++) {
    std::cout << " " << this->ranks[i];
  }
  cout << "\n";
  cout << this->target << endl;

  /* for (int i = 0; i < this->length; i++) { */
  /*   display_string(this->positions[i]); */
  /* } */

  for (auto &h : this->hvec) {
    h.clear();
  }
  this->tmp_ranks.clear();
}

int BurrowsWheelerTransformation::get_char_idx(int idx) {
  if (idx >= this->length) {
    return (idx - this->length);
  } else {
    return idx;
  }
}

void BurrowsWheelerTransformation::display_string(int idx) {
  char * starting_char = this->source + idx;
  char * current_char = starting_char;
  cout << "STRING " << idx << ": ";
  while (true) {
    cout << *current_char;
    current_char++;
    if (current_char == this->source_end) {
      current_char = this->source;
    }
    if (current_char == starting_char) {
      break;
    }
  }
  cout << "\n";
}

/*
 * Poczatek:
 * 0    B A B A C A 3
 * 1    A B A C A B 0
 * 2    B A C A B A 4
 * 3    A C A B A B 1
 * 4    C A B A B A 5
 * 5    A B A B A C 2
 *
 * Iteracja 0:
 * 1    A B A C A B 3
 * 3    A C A B A B 5
 * 5    A B A B A C 4
 * 0    B A B A C A 0
 * 2    B A C A B A 1
 * 4    C A B A B A 2
 * 3 0 4 1 5 2
 *
 *
 * Iteracja 1a:
 * 0    B A B A C A 3
 * 2    B A C A B A 4
 * 4    C A B A B A 5
 * 1    A B A C A B 0
 * 5    A B A B A C 1
 * 3    A C A B A B 2
 * 0 3 1 5 2 4
 *
 *
 * Iteracja 1b:
 * 1    A B A C A B
 * 5    A B A B A C
 * 3    A C A B A B
 * 0    B A B A C A
 * 2    B A C A B A
 * 4    C A B A B A
 * 3 0 4 2 5 1
 */
