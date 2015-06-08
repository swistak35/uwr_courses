#ifndef MOVE_TO_FRONT_H
#define MOVE_TO_FRONT_H

#define MOVE_TO_FRONT_VERBOSE 0

#include <algorithm>
#include <vector>
#include <iostream>
#include <cmath>
#include <list>

template <typename From, typename To>
class MoveToFront {
  public:
    MoveToFront();
    ~MoveToFront();
    void run(From * source, int count);
    void run(int source);
    void reset();
    To * target;
  private:
    To get_char(From c);
    int batchSize;
    std::list<From> table;
};

#endif
