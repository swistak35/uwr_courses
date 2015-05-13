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

  for (int i = 0; i < this->length; i++) {
    this->ranks.push_back(i);
    this->positions.push_back(i);
  }
  
  int offset = this->length - 1;
  while (offset >= 0) {
    vector<vector<int>> hvec(256, vector<int>(0));

    for (int i = 0; i < this->length; i++) {
      char * begin_of_this_string = this->source - this->positions[i];
      if (begin_of_this_string < this->source) {
        begin_of_this_string += this->length;
      }

      char * offseth_char_of_this_string = begin_of_this_string + offset;
      if (offseth_char_of_this_string > source_end) {
        offseth_char_of_this_string -= this->length;
      }
      // na razie wrzucamy tylko nr stringa
      hvec[*offseth_char_of_this_string].push_back = this->positions[i];
    }

    vector<int> new_order(this->length);

    for (auto hvec1 : hvec) {
      for (int string_number : hvec1) {
        new_order.push_back(string_number);
      }
    }


    offset--;
  }
}
