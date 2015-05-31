#include "LexiDeBWT.h"

using namespace std;

LexiDeBWT::LexiDeBWT(int length) {
  this->length = length;
}

LexiDeBWT::~LexiDeBWT() {
}

void LexiDeBWT::transform(int orig_idx, int * source, unsigned char * target) {
  this->source_end = source + (this->length - 1);
  this->source = source;
  this->target = target;

  // pamietac w koncowej implementacji usunac + 1 i dodawanie bajtu zerowego!
  /* this->sorted = (int *) malloc(sizeof(int) * (this->length + 1)); */
  this->sorted = (int *) calloc(this->length + 1, sizeof(int));
  /* this->sorted[this->length - 1] = 0; */

  std::vector<std::vector<int>> hvec;
  std::vector<int> positions;
  hvec.assign(257, std::vector<int>(0));

  for (int i = 0; i < this->length; i++) {
    int c = this->source[i];
    hvec[c].push_back(i);
  }

  {
    int j = 0;
    for (int i = 0; i <= 256; i++) {
      for (int v : hvec[i]) {
        positions.push_back(v);
        this->sorted[j] = i;
        j++;
      }
    }
  }

  /* cout << "Sorted: " << this->sorted << endl; */

  int transformation[this->length];

  for (int i = 0; i < this->length; i++) {
    /* char c = this->sorted[i]; */
    /* int gdzie_on_byl = this->positions[i]; */
    transformation[i] = positions[i];
  }

  {
    int i = orig_idx;

    // first iteration
    unsigned char c = (unsigned char) this->source[i];
    this->target[this->length - 1] = c;
    printf("Przypisywanie `%d` na %d\n", (int) c, this->length - 1);
    i = transformation[i];

    // rest of the iterations
    for (int j = 1; j < this->length; j++) {
      c = (unsigned char) this->source[i];
      this->target[j - 1] = c;
      printf("Przypisywanie `%d` na %d\n", (int) c, j - 1);
      i = transformation[i];
    }
  }
}
