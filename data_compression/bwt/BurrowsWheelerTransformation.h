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
    std::vector<int> ranks; // na ktorym miejscu jest i-ty string
    std::vector<int> tmp_ranks; // na ktorym miejscu jest i-ty string
    std::vector<int> positions; // ktory string jest na i-tym miejscu
    std::vector<std::vector<int>> hvec;
    void initial_sort();
    void sort_iteration_step();
    void display_string(int idx);
    int get_char_idx(int idx);
    int length;
    char * source;
    char * target;
    char * source_end;
};

#endif
