#ifndef LEXI_DEBWT_H
#define LEXI_DEBWT_H

#define LEXI_DEBWT_VERBOSE 0

#include <algorithm>
#include <vector>
#include <iostream>
#include <cmath>

class LexiDeBWT {
  public:
    LexiDeBWT(int length);
    ~LexiDeBWT();
    void transform(int orig_idx, char * source, char * target);
  /* private: */
    /* std::vector<int> ranks; // na ktorym miejscu jest i-ty string */
    /* std::vector<int> positions; // ktory string jest na i-tym miejscu */
    /* std::vector<std::vector<int>> hvec; */
    /* void sort(); */
    /* void display_string(int idx); */
    /* int get_char_idx(int idx); */
    int length;
    char * source;
    char * sorted;
    char * target;
    char * source_end;
};

#endif
