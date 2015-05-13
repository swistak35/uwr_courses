#include "DemoveToFront.h"

using namespace std;

DemoveToFront::DemoveToFront(int length) {
  this->length = length;
}

DemoveToFront::~DemoveToFront() {
}

void DemoveToFront::transform(int * source, char * target) {
  this->source = source;
  this->target = target;

  for (int i = 0; i < 256; i++) {
    this->table.push_back(i);
  }

  {
    for (int i = 0; i < this->length; i++) {
      this->target[i] = get_char(this->source[i]);
    }
  }
}

char DemoveToFront::get_char(int pos) {
  list<char>::iterator it = this->table.begin();

  for (int i = 0; i < pos; i++) {
    it++;
  }

  /* while (it != this->table.end()) { */
    /* if (pos == req_pos) */
    /* if (*it == c) { */
    /*   break; */
    /* } */
    /* pos++; */
    /* it++; */
  /* } */

  char c = *it;
  this->table.erase(it);
  this->table.push_front(c);

  return c;
}
