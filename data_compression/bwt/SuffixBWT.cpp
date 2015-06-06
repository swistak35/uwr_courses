#include "SuffixBWT.h"
#include <iostream>
#include <assert.h>
#include <cstdio>
#include <strings.h>

using namespace std;

SuffixBWT::SuffixBWT(int length) {
  this->length = length;
  this->current_position = 0;
  this->bnode_counter = 0;
  this->bnode_stack = (BranchNode *) calloc(2 * this->length + 4, sizeof(BranchNode));
  this->bnode_stack_ptr = this->bnode_stack;
}

/* SuffixBWT::prepare(int length) { */
/*   if (this->length < length) { */
/*     free(this->bnode_stack); */
/*   } else { */

/*   } */
/*   this->length = length; */
/*   this->current_position = 0; */
/*   this->bnode_stack_ptr = */ 
/* } */

SuffixBWT::~SuffixBWT() {
}

int SuffixBWT::transform(unsigned char * source, int * target) {
  this->source_end = source + (this->length - 1);
  this->source = source;
  this->target = target;

  this->ranks.clear();
  for (int i = 0; i < this->length; i++) {
    this->ranks.push_back(-1);
  }

  this->tree = new SuffixTree(this->length);
  this->tree->initialize(this->source);

  int current_char = -1;
  while (current_char != this->length - 1) {
    current_char++;
    this->tree->insert_next();
  }

  set_ranks_root();

  return this->ranks[0];
}

void SuffixBWT::set_ranks_root() {
  forward_list<pair<BranchNode *, int>> next_nodes_list;
  next_nodes_list.push_front(make_pair(this->tree->root_node, 0));

  BranchNode * node;
  int depth;
  while (!next_nodes_list.empty()) {
    node = next_nodes_list.front().first;
    depth = next_nodes_list.front().second;
    next_nodes_list.pop_front();

    if (node->edges->empty()) {
      int suffix_id = this->length - depth;
      assert(suffix_id >= 0);
      assert(suffix_id < this->length);
      this->ranks[suffix_id] = this->current_position;
      assert(current_position < this->length);
      if (suffix_id == 0) {
        this->target[this->ranks[suffix_id]] = 0;
      } else {
        this->target[this->ranks[suffix_id]] = this->source[suffix_id - 1];
      }
      current_position++;
    } else {
      Edge * edge;
      /* for (list<Edge*>::reverse_iterator it = node->edges->rbegin(); it != node->edges->rend(); it++) { */
      for (map<int,Edge*>::reverse_iterator it = node->edges->rbegin(); it != node->edges->rend(); it++) {
        edge = (*it).second;
        next_nodes_list.push_front(make_pair(edge->target,
              depth + edge->endingChar - edge->startingChar + 1));
      }
    }
  }
}
