#include "SuffixTree.h"
#include <iostream>
#include <assert.h>
#include <cstdio>
#include <strings.h>

using namespace std;

SuffixTree::SuffixTree(int max_length, unsigned char * source) {
  this->max_length = max_length;
  this->source = source;

  this->bnode_memory = new StackBranchNodeMemory(this->max_length);

  this->edge_stack = (Edge *) calloc(2 * this->max_length + 4 + 257, sizeof(Edge));
  this->edge_stack_ptr = this->edge_stack;

  this->leaves = new list<BranchNode*>();
}

SuffixTree::~SuffixTree() {
  delete this->bnode_memory;
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
    leaves->push_back(information_node);

    edge = create_edge();
    edge->target = information_node;
    information_node->incoming = edge;
    edge->startingChar = endingChar;
    edge->endingChar = 1000000000;
    edge->digit = get_digit(this->source + endingChar);
    information_node->parent = bnode;
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
  if (startingChar <= endingChar) { // or endingChar == INFINITY
    int charAtStartingChar = get_digit(this->source + startingChar);
    Edge * edge = find_edge_on_list(current_node, charAtStartingChar);
#ifdef DEBUG
    assert(edge != NULL);
#endif
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
      previous_target->incoming = new_edge;
      previous_target->parent = new_bnode;
      new_edge->digit = get_digit(this->source + new_edge->startingChar);

      edge->endingChar = edge->startingChar + endingChar - startingChar;
      edge->target = new_bnode;
      new_bnode->incoming = edge;
      new_bnode->parent = current_node;

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
  this->bnode_memory->reset();
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

void SuffixTree::remove() {
  // po kazdym dodanym znaku musimy dodac element do leaves
  // ten element to wskaznik na lisc lub na jakis element + przesuniecie
  // jak usuwamy jakis element, zawsze musimy usunac cos z leaves
  assert(!leaves->empty());
  BranchNode * removing_leaf = leaves->front()->second;
  leaves->pop_front();
  BranchNode * parent_node = removing_leaf->parent;

  if (current_node == parent_node) {
    int charAtStartingChar = get_digit(this->source + startingChar);
    Edge * edge = find_edge_on_list(current_node, charAtStartingChar);
    // moglibysmy tu od razu zwracac iterator, usuwanie byloby tansze
    if (edge->target == removing_leaf) {
      return;
      // 
    } else {
      current_node_incoming = current_node->incoming;
      int incoming_length = current_node_incoming->endingChar - current_node_incoming->startingChar + 1;
      current_node = current_node->parent;
      startingChar -= incoming_length;
    }
  } else {
    // delete_node(removing_leaf)
    parent_node->edges->erase(removing_leaf->incoming->digit);
    assert(parent_node->edges->size() >= 1);
    if (parent_node->edges->size() == 1) {
      map<int,Edge*>::iterator it = parent_node->edges->begin();
      Edge * remaining_edge = it->second
      BranchNode * remaining_child = remaining_edge->target;
      Edge * parent_edge = parent_node->incoming;
      BranchNode * grandparent_node = parent_node->parent;
      int parent_edge_length = parent_edge->endingChar - parent_edge->startingChar + 1;
      remaining_edge->startingChar = remaining_edge->startingChar - parent_node_length;
      assert(remaining_edge->startingChar >= 0);
      remaining_child->parent = grandparent_node;
      // delete_node(parent_node)
      // delete_edge(parent_edge)
    } else {
      // we are done? :)
    }
  }
  /* removing node = currently first leaf; */
  /* targeting_insertion_point = */ 
}

void SuffixTree::insert_next() {
  current_char++;

#ifdef DEBUG
  print_file();
  printf("======================================================\n");
  print_file();
  printf("Current character: %d\n", current_char);
  print_file();
  printf("Starting char %d | Current node: %d", startingChar, current_node->debugchar);
#endif

  update(current_char);
  canonize(current_node, current_char);

#ifdef DEBUG
  print_tree(root_node);
#endif

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
  /* ptr->edges = new map<int,Edge*>(); */
  /* ptr->longestProperSuffix = NULL; */
  /* ptr->debugchar = bnode_counter; */

  /* bnode_counter++; */
  /* return ptr; */
  return this->bnode_memory->create();
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
  print_file();
  for (int i = 0; i < depth; i++) {
    printf("-");
  }
}

void SuffixTree::print_tree(BranchNode * node) {
  print_node(1, node);
}

void SuffixTree::print_tree() {
  print_tree(this->root_node);
}

void SuffixTree::print_file() {
  printf("==SuffixTree.cpp== ");
}
