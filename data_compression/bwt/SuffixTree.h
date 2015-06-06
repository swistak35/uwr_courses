#ifndef SUFFIX_TREE_H
#define SUFFIX_TREE_H

#ifndef SUFFIX_TREE_VERBOSE
#define SUFFIX_TREE_VERBOSE 0
#endif

#include <algorithm>
#include <list>
#include <forward_list>
#include <map>
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
  map<int, Edge*> * edges;
  int debugchar;
} BranchNode;

class SuffixTree {
  public:
    SuffixTree(int length);
    ~SuffixTree();
    void initialize(unsigned char * source);
    void insert_next();
    BranchNode * root_node;
  private:
    void sort();
    int startingChar;
    int current_char;
    BranchNode * current_node;

    int get_digit(unsigned char * chr_ptr);
    int length;
    unsigned char * source;
    unsigned char * source_end;

    /* // builder */
    // BranchNodes memory
    BranchNode * bnode_stack;
    BranchNode * bnode_stack_ptr;

    int bnode_counter;
    BranchNode * pin_node;
    BranchNode * create_branch_node();
    Edge * create_edge();
    Edge * find_edge_on_list(BranchNode * node, int c);
    void insert_edge_into_bnode(BranchNode * node, Edge * edge);
    void canonize(BranchNode * node, int endingChar, BranchNode ** result);
    bool test_and_split(BranchNode * node, int startingChar, int endingChar, int current_char,
      BranchNode ** bnode);
    void update(BranchNode * node, int endingChar, BranchNode ** result);

    void print_node(int depth, BranchNode * node);
    void print_tree(BranchNode * root_node);
    void print_tabs(int depth);
};

#endif

