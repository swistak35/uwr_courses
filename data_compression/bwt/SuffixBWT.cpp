#include "SuffixBWT.h"
#include <iostream>
#include <assert.h>
#include <cstdio>
#include <strings.h>

using namespace std;

SuffixBWT::SuffixBWT(int max_length, unsigned char * source, int * target) {
  this->max_length = max_length;

  this->source = source;
  this->target = target;

  this->ranks = (int *) calloc(this->max_length, sizeof(int));
  this->tree = new SuffixTree(this->max_length, this->source);
}

SuffixBWT::~SuffixBWT() {
  delete this->tree;
  free(this->ranks);
}

int SuffixBWT::transform(int length) {
  this->length = length;
  this->tree->initialize(this->length);

  for (int i = 0; i < this->length; i++) {
    this->tree->insert_next();
  }

  set_ranks_root();

  return this->ranks[0];
}

void SuffixBWT::set_ranks_root() {
  int current_position = 0;
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
      this->ranks[suffix_id] = current_position;
      assert(current_position < this->length);
      if (suffix_id == 0) {
        this->target[this->ranks[suffix_id]] = 0;
      } else {
        this->target[this->ranks[suffix_id]] = this->source[suffix_id - 1];
      }
      current_position++;
    } else {
      Edge * edge;
      for (map<int,Edge*>::reverse_iterator it = node->edges->rbegin(); it != node->edges->rend(); it++) {
        edge = (*it).second;
        if (edge->endingChar == 1000000000) {
          next_nodes_list.push_front(make_pair(edge->target,
                depth + this->length - 1 - edge->startingChar + 1));
        } else {
          next_nodes_list.push_front(make_pair(edge->target,
                depth + edge->endingChar - edge->startingChar + 1));
        }
      }
    }
  }
}
