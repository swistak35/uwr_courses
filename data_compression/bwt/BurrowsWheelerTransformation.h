#ifndef BWT_H
#define BWT_H

#include <algorithm>
#include <vector>
#include <iostream>
#include <cmath>

class BurrowsWheelerTransformation {
  public:
    BurrowsWheelerTransformation(int length);
    ~BurrowsWheelerTransformation();
    int transform(char * source, char * target);
  /* private: */
    std::vector<int> ranks;
    void initial_sort();
    int length;
    char * source;
    char * target;
};

#endif
