#ifndef SUFFIX_BWT_H
#define SUFFIX_BWT_H

#define SUFFIX_BWT_VERBOSE 0

#include <algorithm>
#include <list>
#include <vector>
#include <iostream>
#include <cmath>

using namespace std;

typedef struct {
  int digit;
  int startingChar;
  int endingChar;
  struct BranchNode * target;
} Edge;

typedef struct BranchNode {
  struct BranchNode * longestProperSuffix;
  list<Edge*> * edges;
  int suffix_id;
  int depth;
  char debugchar;
} BranchNode;

typedef struct InformationNode {
  int id;
} InformationNode;


class SuffixBWT {
  public:
    SuffixBWT(int length);
    ~SuffixBWT();
    int transform(unsigned char * source, int * target);
  /* private: */
    std::vector<int> ranks; // na ktorym miejscu jest i-ty string
    /* std::vector<int> positions; // ktory string jest na i-tym miejscu */
    /* std::vector<std::vector<int>> hvec; */
    void sort();
    int get_char_idx(int idx);
    int length;
    unsigned char * source;
    int * target;
    unsigned char * source_end;
    void set_ranks(BranchNode * node);

    // builder
    BranchNode * create_branch_node();
    InformationNode * create_information_node();
    Edge * create_edge();
    Edge * find_edge_on_list(BranchNode * node, int c);
    void insert_edge_into_bnode(BranchNode * node, Edge * edge);

    void print_node(int depth, BranchNode * node);
    void print_tree(BranchNode * root_node);
    void print_tabs(int depth);
};

#endif
