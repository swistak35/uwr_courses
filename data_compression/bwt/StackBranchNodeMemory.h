#ifndef STACK_BRANCH_NODE_MEMORY_H
#define STACK_BRANCH_NODE_MEMORY_H

#include <algorithm>
#include <cstdio>
#include <map>
#include "SuffixTreeStructures.h"

using namespace std;

class StackBranchNodeMemory {
  public:
    StackBranchNodeMemory(int max_length);
    ~StackBranchNodeMemory();
    void reset();
    BranchNode * create_bnode();
    Edge * create_edge();
  private:
    int max_length;

    // BranchNodes
    int bnodes_count;
    int bnodes_counter;
    BranchNode * bnodes_stack;
    BranchNode * bnodes_stack_ptr;

    // Edges
    int edges_count;
    Edge * edges_stack;
    Edge * edges_stack_ptr;
};

#endif
