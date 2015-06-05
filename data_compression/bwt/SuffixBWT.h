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

class SuffixBWT {
  public:
    SuffixBWT(int length);
    ~SuffixBWT();
    int transform(unsigned char * source, int * target);
  private:
    std::vector<int> ranks; // na ktorym miejscu jest i-ty string
    std::vector<int> positions; // ktory string jest na i-tym miejscu
    void sort();

    int get_digit(unsigned char * chr_ptr);
    int length;
    unsigned char * source;
    int * target;
    unsigned char * source_end;
    void set_ranks_root();
    int current_position;

    // builder
    BranchNode * bnode_stack;
    BranchNode * bnode_stack_ptr;
    int bnode_counter;
    BranchNode * root_node;
    BranchNode * pin_node;
    BranchNode * create_branch_node();
    Edge * create_edge();
    Edge * find_edge_on_list(BranchNode * node, int c);
    void insert_edge_into_bnode(BranchNode * node, Edge * edge);
    int canonize(BranchNode * node, int startingChar, int endingChar, BranchNode ** result);
    bool test_and_split(BranchNode * node, int startingChar, int endingChar, int current_char,
      BranchNode ** bnode);
    int update(BranchNode * node, int startingChar, int endingChar, BranchNode ** result);

    void print_node(int depth, BranchNode * node);
    void print_tree(BranchNode * root_node);
    void print_tabs(int depth);
};

// optymalizacje:
// - stack dla edge
// - stack dla par przy set_ranks_root
// - ifdef ifndef
// - obwarowac ifdefami asserty
// - inicjalizacja wszystkich map na poczatku
// - valgrind!
// - tylko raz inicjalizujemy SuffixBWT

#endif
