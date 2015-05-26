#include <list>
#include <vector>
#include <cstdlib>
#include <cstdio>

using namespace std;

typedef struct {
  int digit;
} Edge;

typedef struct {
  list< Edge* > * edges;
} BranchNode;


int main() {
  /* BranchNode * ptr = (BranchNode *) calloc(1, sizeof(BranchNode)); */
  BranchNode * ptr = (BranchNode *) malloc(sizeof(BranchNode));
  ptr->edges = new list<Edge*>();
  printf("CO jest kurwa %d\n", ptr->edges->size());

  return 0;
}
