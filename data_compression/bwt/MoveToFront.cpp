#include "MoveToFront.h"

using namespace std;

MoveToFront::MoveToFront(int length) {
  this->length = length;
}

MoveToFront::~MoveToFront() {
}

void MoveToFront::transform(char * source, int * target) {
  this->source = source;
  this->target = target;

  for (int i = 0; i < 256; i++) {
    this->table.push_back(i);
  }

  {
    /* char * current_char = this->source; */
    for (int i = 0; i < this->length; i++) {
      this->target[i] = get_char(this->source[i]);
    }
  }
}

int MoveToFront::get_char(char c) {
  list<char>::iterator it = this->table.begin();

  int pos = 0;
  while (it != this->table.end()) {
    if (*it == c) {
      break;
    }
    pos++;
    it++;
  }

  this->table.erase(it);
  this->table.push_front(c);

  return pos;
}
