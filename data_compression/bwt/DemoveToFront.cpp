#include "DemoveToFront.h"

using namespace std;

DemoveToFront::DemoveToFront() {
  this->reset();
}

DemoveToFront::~DemoveToFront() {
}

void DemoveToFront::reset() {
  this->table.clear();
  for (int i = 0; i < 256; i++) {
    this->table.push_back(i);
  }
}

void DemoveToFront::run(int * i) {
  *i = 0;
  int ti;
  char c;
  for (int j = 0; j < 4; j++) {
    c = get_char(*this->source);
    cout << "Dobrałem bajt: `" << +c << "`\n";
    this->source++;
    ti = c << (8 * (4 - j));
    *i = *i | ti;
  }
}

void DemoveToFront::run(char * target, int count) {
  for (int i = 0; i < count; i++) {
    target[i] = get_char(*this->source);
    this->source++;
  }
}

char DemoveToFront::get_char(int pos) {
  list<char>::iterator it = this->table.begin();

  for (int i = 0; i < pos; i++) {
    it++;
  }

  char c = *it;
  this->table.erase(it);
  this->table.push_front(c);

  return c;
}
