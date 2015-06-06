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
    SuffixBWT(int max_length, unsigned char * source, int * target);
    ~SuffixBWT();
    int transform(int length);
  private:
    SuffixTree * tree;

    int length;
    int max_length;
    unsigned char * source;
    int * target;
    int set_ranks_root();
};

// optymalizacje:
// - stack dla par przy set_ranks_root
// - ifdef ifndef
// - obwarowac ifdefami asserty
// - templates i korzystanie z unsigned char gdzie sie da, albo ogolnie mniejszych typow

#endif
