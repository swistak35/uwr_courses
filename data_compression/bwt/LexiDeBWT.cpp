#include "LexiDeBWT.h"

using namespace std;

LexiDeBWT::LexiDeBWT(int length) {
  this->length = length;
}

LexiDeBWT::~LexiDeBWT() {
}

void LexiDeBWT::transform(int orig_idx, char * source, char * target) {
  this->source_end = source + (this->length);
  this->source = source;
  this->target = target;

  // pamietac w koncowej implementacji usunac + 1 i dodawanie bajtu zerowego!
  this->sorted = (char *) malloc(this->length + 1);
  this->sorted[this->length - 1] = 0;

  std::vector<std::vector<int>> hvec;
  std::vector<int> positions;
  hvec.assign(256, std::vector<int>(0));

  for (int i = 0; i < this->length; i++) {
    char c = this->source[i];
    hvec[c].push_back(i);
  }

  {
    int j = 0;
    for (int i = 0; i < 256; i++) {
      for (int v : hvec[i]) {
        positions.push_back(v);
        this->sorted[j] = i;
        j++;
      }
    }
  }

  cout << "Sorted: " << this->sorted << endl;

  int transformation[this->length];

  for (int i = 0; i < this->length; i++) {
    /* char c = this->sorted[i]; */
    /* int gdzie_on_byl = this->positions[i]; */
    transformation[i] = positions[i];
  }

  {
    int i = orig_idx;

    // first iteration
    this->target[this->length - 1] = this->source[i];
    i = transformation[i];

    // rest of the iterations
    for (int j = 1; j < this->length; j++) {
      this->target[j - 1] = this->source[i];
      i = transformation[i];
    }
  }
}

/* int LexiDeBWT::get_char_idx(int idx) { */
/*   if (idx >= this->length) { */
/*     return (idx - this->length); */
/*   } else { */
/*     return idx; */
/*   } */
/* } */

/* void LexiDeBWT::display_string(int idx) { */
/*   char * starting_char = this->source + idx; */
/*   char * current_char = starting_char; */
/*   cout << "STRING " << idx << ": "; */
/*   while (true) { */
/*     cout << *current_char; */
/*     current_char++; */
/*     if (current_char == this->source_end) { */
/*       current_char = this->source; */
/*     } */
/*     if (current_char == starting_char) { */
/*       break; */
/*     } */
/*   } */
/*   cout << "\n"; */
/* } */
