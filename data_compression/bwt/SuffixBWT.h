#ifndef SUFFIX_BWT_H
#define SUFFIX_BWT_H

#ifndef SUFFIX_BWT_VERBOSE
#define SUFFIX_BWT_VERBOSE 0
#endif

#include <algorithm>
#include <list>
#include <forward_list>
#include <map>
#include <vector>
#include <iostream>
#include <cmath>
#include "SuffixTree.h"

using namespace std;

class SuffixBWT {
  public:
    SuffixBWT(int length);
    ~SuffixBWT();
    int transform(unsigned char * source, int * target);
  private:
    std::vector<int> ranks;
    SuffixTree * tree;

    int get_digit(unsigned char * chr_ptr);
    int length;
    unsigned char * source;
    int * target;
    unsigned char * source_end;
    void set_ranks_root();
    int current_position;
};

// optymalizacje:
// - stack dla edge
// - stack dla par przy set_ranks_root
// - ifdef ifndef
// - obwarowac ifdefami asserty
// - inicjalizacja wszystkich map na poczatku
// - valgrind!
// - tylko raz inicjalizujemy SuffixBWT
// - templates i korzystanie z unsigned char gdzie sie da, albo ogolnie mniejszych typow

#endif
