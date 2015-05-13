#ifndef LEXI_BWT_H
#define LEXI_BWT_H

#define LEXI_BWT_VERBOSE 0

#include <algorithm>
#include <vector>
#include <iostream>
#include <cmath>

class LexiBWT {
  public:
    LexiBWT(int length);
    ~LexiBWT();
    int transform(char * source, char * target);
  /* private: */
    std::vector<int> ranks; // na ktorym miejscu jest i-ty string
    std::vector<int> positions; // ktory string jest na i-tym miejscu
    std::vector<std::vector<int>> hvec;
    void sort();
    void display_string(int idx);
    int get_char_idx(int idx);
    int length;
    char * source;
    char * target;
    char * source_end;
};

#endif
