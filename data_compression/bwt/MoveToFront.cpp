#include "MoveToFront.h"

using namespace std;

MoveToFront::MoveToFront() {
  this->reset();
}

MoveToFront::~MoveToFront() {
}

void MoveToFront::reset() {
  this->table.clear();

  for (int i = 0; i < 256; i++) {
    this->table.push_back(i);
  }
}

void MoveToFront::run(char * source, int count) {
  for (int i = 0; i < count; i++) {
    *this->target = get_char(source[i]);
    this->target++;
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
