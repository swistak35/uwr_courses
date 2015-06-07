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
    BranchNode * create();
    void reset();
  private:
    int max_length;
    int count;
    int counter;
    BranchNode * stack;
    BranchNode * stack_ptr;
};

#endif
