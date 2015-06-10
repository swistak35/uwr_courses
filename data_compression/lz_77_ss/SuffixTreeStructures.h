#ifndef SUFFIX_TREE_STRUCTURES
#define SUFFIX_TREE_STRUCTURES

#include <map>

using namespace std;

typedef struct {
  int digit;
  int startingChar;
  int endingChar;
  struct BranchNode * target;
} Edge;

typedef struct BranchNode {
  struct BranchNode * longestProperSuffix;
  struct BranchNode * parent;
  Edge * incoming;
  map<int, Edge*> * edges;
  int debugchar;
} BranchNode;

#endif
