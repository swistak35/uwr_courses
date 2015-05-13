#ifndef MOVE_TO_FRONT_H
#define MOVE_TO_FRONT_H

#define MOVE_TO_FRONT_VERBOSE 0

#include <algorithm>
#include <vector>
#include <iostream>
#include <cmath>
#include <list>

class MoveToFront {
  public:
    MoveToFront(int length);
    ~MoveToFront();
    void transform(char * source, int * target);
    int get_char(char c);

    int length;
    char * source;
    int * target;
    std::list<char> table;
};

#endif
