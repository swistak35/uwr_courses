#ifndef DEMOVE_TO_FRONT_H
#define DEMOVE_TO_FRONT_H

#define DEMOVE_TO_FRONT_VERBOSE 0

#include <algorithm>
#include <vector>
#include <iostream>
#include <cmath>
#include <list>

class DemoveToFront {
  public:
    DemoveToFront(int length);
    ~DemoveToFront();
    void transform(int * source, char * target);
    char get_char(int pos);

    int length;
    int * source;
    char * target;
    std::list<char> table;
};

#endif
