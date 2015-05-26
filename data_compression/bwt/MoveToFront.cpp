#include "MoveToFront.h"

using namespace std;

MoveToFront::MoveToFront() {
  this->reset();
}

MoveToFront::~MoveToFront() {
}

void MoveToFront::reset() {
  this->table.clear();

  for (int i = 0; i <= 256; i++) {
    this->table.push_back(i);
  }
}

void MoveToFront::run(int i) {
  unsigned int mask = 255;
  int si = i;
  unsigned char c;
  for (int j = 0; j < 4; j++) {
    c = si & mask;
    *this->target = get_char((int)c);
    this->target++;
    si = si >> 8;
  }
}

void MoveToFront::run(char * source, int count) {
  for (int i = 0; i < count; i++) {
    /* cout << "Ruszamy " << i << endl; */
    int znak = int(source[i]);
    /* cout << "Znak " << +source[i] << " casted to " << znak << endl; */
    *this->target = get_char(znak);
    this->target++;
  }
}

void MoveToFront::run(int * source, int count) {
  for (int i = 0; i < count; i++) {
    cout << "Ruszamy " << i << endl;
    *this->target = get_char(source[i]);
    cout << "Znak " << source[i] << endl;
    this->target++;
  }
}

int MoveToFront::get_char(int c) {
  list<int>::iterator it = this->table.begin();

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
