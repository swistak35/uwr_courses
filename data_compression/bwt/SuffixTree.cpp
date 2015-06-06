#include "SuffixTree.h"
#include <iostream>
#include <assert.h>
#include <cstdio>
#include <strings.h>

using namespace std;

SuffixTree::SuffixTree(int max_length, unsigned char * source) {
  this->max_length = max_length;
  this->source = source;

  // BranchNodes memory
  this->bnode_stack = (BranchNode *) calloc(2 * this->max_length + 4, sizeof(BranchNode));
  this->bnode_stack_ptr = this->bnode_stack;
  for (int i = 0; i < 2 * this->max_length + 3; i++) {
    this->bnode_stack[i].edges = new map<int,Edge*>();
  }

  this->edge_stack = (Edge *) calloc(2 * this->max_length + 4 + 257, sizeof(Edge));
  this->edge_stack_ptr = this->edge_stack;
}

SuffixTree::~SuffixTree() {
  for (int i = 0; i < 2 * this->max_length + 4; i++) {
    delete bnode_stack[i].edges;
  }
  free(this->bnode_stack);
  free(this->edge_stack);
}

void SuffixTree::update(int endingChar) {
  BranchNode * oldr = this->root_node;
  BranchNode * information_node = NULL;
  BranchNode * bnode = NULL;
  int current_char = get_digit(this->source + endingChar);
  bool end_point = test_and_split(endingChar - 1, current_char, &bnode);
  Edge * edge = NULL;

  while (!end_point) {
    information_node = create_branch_node();

    edge = create_edge();
    edge->target = information_node;
    edge->startingChar = endingChar;
    edge->endingChar = 1000000000;
    edge->digit = get_digit(this->source + endingChar);
    insert_edge_into_bnode(bnode, edge);

    if (oldr != this->root_node) {
      oldr->longestProperSuffix = bnode;
    }
    oldr = bnode;
    canonize(current_node->longestProperSuffix, endingChar - 1);
    end_point = test_and_split(endingChar - 1, current_char, &bnode);
  }

  if (oldr != this->root_node) {
    oldr->longestProperSuffix = current_node;
  }
}

bool SuffixTree::test_and_split(int endingChar, int current_char, BranchNode ** bnode) {
  if (SUFFIX_TREE_VERBOSE) {
    printf("== test_and_split node=%d startingChar=%d endingChar=%d current_char=%d\n",
        current_node->debugchar, startingChar, endingChar, current_char);
  }
  if (startingChar <= endingChar) { // or endingChar == INFINITY
    int charAtStartingChar = get_digit(this->source + startingChar);
    Edge * edge = find_edge_on_list(current_node, charAtStartingChar);
    assert(edge != NULL);
    int forward_char = get_digit(this->source + edge->startingChar + endingChar - startingChar + 1);
    if (current_char == forward_char) {
      *bnode = current_node;
      return true;
    } else {
      BranchNode * new_bnode = create_branch_node();
      BranchNode * previous_target = edge->target;

      Edge * new_edge = create_edge();
      new_edge->startingChar = edge->startingChar + endingChar - startingChar + 1;
      new_edge->endingChar = edge->endingChar;
      new_edge->target = previous_target;
      new_edge->digit = get_digit(this->source + new_edge->startingChar);

      edge->endingChar = edge->startingChar + endingChar - startingChar;
      edge->target = new_bnode;

      insert_edge_into_bnode(new_bnode, new_edge);

      *bnode = new_bnode;
      return false;
    }
  } else {
    Edge * edge = find_edge_on_list(current_node, current_char);
    *bnode = current_node;
    if (edge == NULL) {
      return false;
    } else {
      return true;
    }
  }
}

void SuffixTree::canonize(BranchNode * node, int endingChar) {
  if (endingChar < startingChar) { // pamietac o nieskonczonosci
    current_node = node;
  } else {
    int looking_char = get_digit(this->source + startingChar);
    Edge * edge = find_edge_on_list(node, looking_char);
    while (edge->endingChar - edge->startingChar <= endingChar - startingChar) {
      startingChar = startingChar + edge->endingChar - edge->startingChar + 1;
      node = edge->target;
      if (startingChar <= endingChar) {
        looking_char = get_digit(this->source + startingChar);
        edge = find_edge_on_list(node, looking_char);
      }
    }

    current_node = node;
  }
}

void SuffixTree::initialize(int length) {
  this->length = length;
  this->bnode_counter = 0;
  this->bnode_stack_ptr = this->bnode_stack;
  this->edge_stack_ptr = this->edge_stack;
  this->source_end = source + (this->length - 1);

  BranchNode * root_node = create_branch_node();
  this->root_node = root_node;

  BranchNode * pin_node = create_branch_node();
  this->pin_node = pin_node;
  for (int i = 0; i <= 256; i++) { // ewidentnie wystarczy tylko jedna!
    Edge * edge = create_edge();
    edge->startingChar = -i;
    edge->endingChar = -i;
    edge->target = root_node;
    edge->digit = i;
    insert_edge_into_bnode(pin_node, edge);
  }

  root_node->longestProperSuffix = pin_node;

  this->current_node = root_node;
  this->startingChar = 0;
  this->current_char = -1;
}

/* void SuffixTree::remove() { */
/*   removing_node = currently_first_leaf; */
/*   targeting_insertion_point = */ 
/* } */

void SuffixTree::insert_next() {
    current_char++;
    if (SUFFIX_TREE_VERBOSE) {
      cout << "==============================================================" << endl;
      cout << "== Zaczynamy z " << current_char << endl;
      cout << "== Starting char " << startingChar << " | Current node: " << current_node->debugchar << endl;
    }
    update(current_char);
    canonize(current_node, current_char);

    if (SUFFIX_TREE_VERBOSE) {
      print_tree(root_node);
    }
}

int SuffixTree::get_digit(unsigned char * chr_ptr) {
  if (chr_ptr == this->source_end) {
    return 256;
  } else {
    return ((int) *chr_ptr);
  }
}

Edge * SuffixTree::find_edge_on_list(BranchNode * node, int c) {
  std::map<int,Edge*>::iterator it;
  it = node->edges->find(c);
  if (it == node->edges->end()) {
    return NULL;
  } else {
    return (*it).second;
  }
}

void SuffixTree::insert_edge_into_bnode(BranchNode * bnode, Edge * edge) {
  bnode->edges->insert(pair<int,Edge*>(edge->digit, edge));
}

// poinicjalizowac na 0, null itp.
BranchNode * SuffixTree::create_branch_node() {
  /* BranchNode * ptr = (BranchNode *) malloc(sizeof(BranchNode)); */
  /* assert(ptr != NULL); */
  BranchNode * ptr = bnode_stack_ptr;
  /* ptr->edges = new map<int,Edge*>(); */
  /* for (std::map<int,Edge*>::iterator it = ptr->edges->begin(); */
      /* it != ptr->edges->end(); it++) { */
    /* free((*it).second); */
  /* } */
  ptr->edges->clear();
  ptr->longestProperSuffix = NULL;
  ptr->debugchar = bnode_counter;

  bnode_counter++;
  bnode_stack_ptr++;
  return ptr;
}

Edge * SuffixTree::create_edge() {
  /* Edge * ptr = (Edge *) malloc(sizeof(Edge)); */
  /* assert(ptr != NULL); */
  Edge * ptr = edge_stack_ptr;
  edge_stack_ptr++;
  return ptr;
}




// Debugging functions

void SuffixTree::print_node(int depth, BranchNode * node) {
  print_tabs(depth);
  if (node->longestProperSuffix == NULL) {
    printf("BranchNode< %d > [ nil ]\n",
        node->debugchar);
  } else {
    printf("BranchNode< %d > [ %d ]\n",
        node->debugchar,
        node->longestProperSuffix->debugchar);
  }
  Edge * edge;
  for (map<int,Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = (*it).second;
    print_tabs(depth);
    assert(edge->digit >= 0);
    assert(edge->digit <= 256);
    printf("Edge ->%d (%d) [%d .. %d] <>\n",
        edge->target->debugchar,
        edge->digit,
        edge->startingChar, edge->endingChar);
  }
  for (map<int,Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = (*it).second;
    print_node(depth + 1, edge->target);
  }
}

void SuffixTree::print_tabs(int depth) {
  for (int i = 0; i < depth; i++) {
    printf("-");
  }
}

void SuffixTree::print_tree(BranchNode * node) {
  print_node(1, node);
}
