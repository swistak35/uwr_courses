#include "MoveToFront.h"

using namespace std;

template <typename From, typename To>
MoveToFront<From,To>::MoveToFront() {
  this->reset();
}

template <typename From, typename To>
MoveToFront<From,To>::~MoveToFront() {
}

template <typename From, typename To>
void MoveToFront<From,To>::reset() {
  this->table.clear();

  for (int i = 0; i < 256; i++) {
    this->table.push_back(i);
  }
}

template <typename From, typename To>
void MoveToFront<From,To>::run(int i) {
  unsigned int mask = 255;
  int si = i;
  unsigned char c;
  for (int j = 0; j < 4; j++) {
    c = si & mask;
    *this->target = get_char((From) c);
    this->target++;
    si = si >> 8;
  }
}

template <typename From, typename To>
void MoveToFront<From,To>::run(From * source, int count) {
  for (int i = 0; i < count; i++) {
    *this->target = get_char(source[i]);
    this->target++;
  }
}

template <typename From, typename To>
To MoveToFront<From,To>::get_char(From c) {
  typename list<From>::iterator it = this->table.begin();

  To pos = 0;
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
