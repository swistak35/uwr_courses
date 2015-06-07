#include "StackBranchNodeMemory.h"

StackBranchNodeMemory::StackBranchNodeMemory(int max_length) {
  this->max_length = max_length;
  this->count = 2 * max_length + 4;

  this->stack = (BranchNode *) calloc(this->count, sizeof(BranchNode));
  if (this->stack == NULL) {
    printf("Malloc failed for StackBranchNodeMemory#stack\n");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < this->count; i++) {
    this->stack[i].edges = new map<int,Edge*>();
  }
}

StackBranchNodeMemory::~StackBranchNodeMemory() {
  for (int i = 0; i < this->count; i++) {
    delete stack[i].edges;
  }
  free(this->stack);
}

BranchNode * StackBranchNodeMemory::create() {
  BranchNode * ptr = stack_ptr;
  ptr->edges->clear();
  ptr->longestProperSuffix = NULL;
  ptr->debugchar = counter;

  counter++;
  stack_ptr++;
  return ptr;
}

void StackBranchNodeMemory::reset() {
  this->counter = 0;
  this->stack_ptr = this->stack;
}
