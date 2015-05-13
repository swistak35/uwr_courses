#include "LexiBWT.h"

using namespace std;

LexiBWT::LexiBWT(int length) {
  this->length = length;
  this->hvec.assign(256, std::vector<int>(0));
}

LexiBWT::~LexiBWT() {
}

int LexiBWT::transform(char * source, char * target) {
  this->source_end = source + (this->length);
  this->source = source;
  this->target = target;

  sort();

  return this->ranks[0];
}

void LexiBWT::sort() {
  // Init vectors
  for (int i = 0; i < this->length; i++) {
    this->ranks.push_back(0);
    this->positions.push_back(i);
  }

  int it = this->length - 1;
  while (it >= 0) {
    for (auto &h : this->hvec) {
      h.clear();
    }

    for (int i = 0; i < this->length; i++) {
      char c = this->source[get_char_idx(this->positions[i] + it)];
      this->hvec[c].push_back(this->positions[i]);
    }

    this->positions.clear();
    for (int i = 0; i < 256; i++) {
      for (int v : this->hvec[i]) {
        this->positions.push_back(v);
      }
    }

    for (int i = 0; i < this->length; i++) {
      this->ranks[this->positions[i]] = i;
    }

    for (int i = 0; i < this->length; i++) {
      this->target[this->ranks[i]] = this->source[i];
    }

    if (LEXI_BWT_VERBOSE) {
      cout << "Ranks:";
      for (int i = 0; i < this->length; i++) {
        std::cout << " " << this->ranks[i];
      }
      cout << "\n";

      cout << "Target: " << this->target << endl;

      for (int i = 0; i < this->length; i++) {
        display_string(this->positions[i]);
      }
    }

    it--;
  }
}


int LexiBWT::get_char_idx(int idx) {
  if (idx >= this->length) {
    return (idx - this->length);
  } else {
    return idx;
  }
}

void LexiBWT::display_string(int idx) {
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
