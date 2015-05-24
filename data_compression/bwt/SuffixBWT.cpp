#include "SuffixBWT.h"
#include <iostream>
#include <assert.h>
#include <cstdio>
#include <strings.h>

using namespace std;

SuffixBWT::SuffixBWT(int length) {
  this->length = length;
  /* this->hvec.assign(256, std::vector<int>(0)); */
}

SuffixBWT::~SuffixBWT() {
}

int SuffixBWT::transform(char * source, char * target) {
  this->source_end = source + (this->length);
  this->source = source;
  this->target = target;

  /* this->ranks.clear(); */
  /* for (int i = 0; i < this->length; i++) { */
  /*   this->ranks.push_back(-1); */
  /* } */

  sort();
  /* print_sorted(); */

  /* return this->ranks[0]; */
  return 0;
}

#define LIMITING_STEPS 4

void SuffixBWT::sort() {
  // init
  char * current_char = this->source;
  char * looking_char;
  BranchNode * root_node = create_branch_node();
  root_node->longestProperSuffix = root_node;
  BranchNode * active_node = root_node;
  int active_length = 0;
  int min_distance = 0;
  BranchNode * nodes[this->length + 1];
  // are they all null?
  
  Edge * next_edge, * edge_to_previous_target;
  BranchNode * new_bnode;
  for (int i = 1; i <= this->length && i <= LIMITING_STEPS; i++) {
    printf("======================================= %d\n", i);
    // utworz information node dla tego suffixa
    nodes[i] = create_branch_node();
    nodes[i]->suffix_id = i;

    // utworz krawedz dla nastepnego information node
    Edge * edge_to_suffix = create_edge();
    edge_to_suffix->target = nodes[i];

    looking_char = current_char + active_length;
    next_edge = find_edge_on_list(active_node, *looking_char);
    if (next_edge == NULL) {
      edge_to_suffix->digit = (int) *looking_char;
      edge_to_suffix->startingChar = active_length + i - 1;
      edge_to_suffix->endingChar = this->length - 1;
      active_node->edges.push_back(edge_to_suffix);
    } else {
      int j = 1;
      looking_char++;
      char * other_char = this->source + next_edge->startingChar + j;
      bool found = false;

      while (!found) {
        while (next_edge->startingChar + j < next_edge->endingChar) {
          other_char++;
          looking_char++;
          j++;
          if (*other_char != *looking_char) {
            break;
          }
        }

        if (*other_char != *looking_char) {
          found = true;
        } else {
          /* active_node = edge->target; */
          /* active_length = edge->endingChar; */
          /* next_edge = find_edge_on_list(active_node, *looking_char); */

          // update active_length?
        }
      }

      edge_to_previous_target = create_edge();
      edge_to_previous_target->digit = (int) *other_char;
      edge_to_previous_target->target = next_edge->target;
      edge_to_previous_target->startingChar = next_edge->startingChar + j;
      edge_to_previous_target->endingChar = next_edge->endingChar;
      edge_to_suffix->digit = (int) *looking_char;
      edge_to_suffix->startingChar = i + j - 1;
      edge_to_suffix->endingChar = this->length - 1;
      new_bnode = create_branch_node();
      new_bnode->edges.push_back(edge_to_previous_target);
      new_bnode->edges.push_back(edge_to_suffix);

      next_edge->endingChar = j-1;
      next_edge->target = new_bnode;

      if (i > 3) {
        nodes[i-1]->longestProperSuffix = nodes[i];
      }
    }

    // ...

    current_char++;
    print_tree(root_node);
  }

  /* set_ranks(root_node); */

  // insert first node
  /* Edge * edge = create_edge(); */
  /* edge->digit = (int) *current_char; */
  /* edge->startingChar = 0; */
  /* edge->endingChar = this->length - 1; */
  /* nodes[0] = create_information_node(); */
  /* nodes[0]->id = 0; */
  /* active_node->edges.push_back(edge); */
  /* current_char++; */

  /* // insert 2nd node */
  /* looking_char = current_char + active_length; // 0 :) */   
  /* Edge * next_step = find_edge_on_list(active_node, *looking_char); */
  /* if (next_step == NULL) { */
  /*   edge = create_edge(); */
  /*   edge->digit = (int) *current_char; */
  /*   edge->startingChar = 1; */
  /*   edge->endingChar = this->length - 1; */
  /*   nodes[1] = create_information_node(); */
  /*   nodes[1]->id = 1; */
  /*   active_node->edges.push_back(edge); */
  /* } */
  /* current_char++; */

  /* // insert 3rd node */
  /* int suffix_id = 3; */
  /* looking_char = current_char + active_length; */
  /* next_step = find_edge_on_list(active_node, *looking_char); */
  /* Edge * new_edge1; */
  /* Edge * new_edge2; */
  /* BranchNode * previous_node; */
  /* BranchNode * new_branch_node; */
  /* InformationNode * new_information_node; */
  /* int lastBranchIndex; */
  /* // next_step is not null */
  /* if (next_step == NULL) { */
  /* } else { */
  /*   int j = 1; */
  /*   char * c1; */
  /*   char * c2; */
  /*   while (j < this->length) { // wlasciwie to ten warunek inny powinien byc */
  /*     c1 = this->source + next_step->startingChar + j; */
  /*     c2 = current_char + j; */
  /*     if (*c1 == *c2) { */
  /*       break; */
  /*     } */
  /*   } */
  /*   lastBranchIndex = suffix_id + next_step->startingChar + j; */
  /*   min_distance = lastBranchIndex - active_length - suffix_id; */
  /*   new_branch_node = create_branch_node(); */
  /*   new_information_node = create_information_node(); */
  /*   if (next_step->endingChar == this->length-1) { */
  /*     InformationNode * previous_node = next_step->target.inode; */
  /*     next_step->endingChar = j - 1; */
  /*     next_step->target.bnode = new_branch_node; */
  /*     new_edge1 = create_edge(); */
  /*     new_edge1->digit = (int) *c1; */
  /*     new_edge1->startingChar = j; */
  /*     new_edge1->endingChar = this->length - 1; */
  /*     new_edge1->target.inode = previous_node; */
  /*     new_edge2 = create_edge(); */
  /*     new_edge2->digit = (int) *c2; */
  /*     new_edge2->startingChar = j; */
  /*     new_edge2->endingChar = this->length - 1; */
  /*     new_edge2->target.inode = new_information_node; */
  /*   } */
  /*   new_information_node->id = suffix_id - 1; */
  /*   nodes[2] = new_information_node; */
  /*   new_branch_node->edges.push_back(new_edge1); */
  /*   new_branch_node->edges.push_back(new_edge2); */
  /* } */
  /* current_char++; */
  
  /* // insert 4th node */
  
  /* //follow the node, may active_length change */
  /* //ensure that new lbr is at least as big as previous one */
  /* suffix_id = 4; */
  /* if (suffix_id + min_distance + active_length > lastBranchIndex) { */
  /*   min_distance = lastBranchIndex - active_length - suffix_id; */
  /* } */
  /* assert(min_distance > 0); */
  
}

/* void SuffixBWT::set_ranks(BranchNode * node) { */
/*   if (node->edges.size == 0) { */
/*   } */ 
/* } */

void SuffixBWT::print_tabs(int depth) {
  for (int i = 0; i < depth; i++) {
    printf("-");
  }
}

void SuffixBWT::print_tree(BranchNode * node) {
  print_node(1, node);
}

void SuffixBWT::print_node(int depth, BranchNode * node) {
  print_tabs(depth);
  printf("BranchNode< %p > (%d) [ %p ]\n",
      node,
      node->suffix_id,
      node->longestProperSuffix);
  Edge * edge;
  for (int i = 0; i < node->edges.size(); i++) {
    edge = node->edges[i];
    print_tabs(depth);
    printf("Edge< %p > (%d) [%d .. %d] -> %p\n",
        edge,
        edge->digit,
        edge->startingChar, edge->endingChar,
        edge->target);
    print_node(depth + 1, edge->target);
  }
}

Edge * SuffixBWT::find_edge_on_list(BranchNode * node, int c) {
  for (vector<Edge*>::iterator it = node->edges.begin(); it != node->edges.end(); it++) {
    if ((*it)->digit == c) {
      return *it;
    }
  }
  return NULL;
}

// poinicjalizowac na 0, null itp.
BranchNode * SuffixBWT::create_branch_node() {
  BranchNode * ptr = (BranchNode *) calloc(1, sizeof(BranchNode));
  bzero(ptr, sizeof(BranchNode));
  return ptr;
}

InformationNode * SuffixBWT::create_information_node() {
  InformationNode * ptr = (InformationNode *) calloc(1, sizeof(InformationNode));
  bzero(ptr, sizeof(InformationNode));
  return ptr;
}

Edge * SuffixBWT::create_edge() {
  Edge * ptr = (Edge *) calloc(1, sizeof(Edge));
  bzero(ptr, sizeof(Edge));
  return ptr;
}

int SuffixBWT::get_char_idx(int idx) {
  if (idx >= this->length) {
    return (idx - this->length);
  } else {
    return idx;
  }
}

void SuffixBWT::display_string(int idx) {
  char * starting_char = this->source + idx;
  char * current_char = starting_char;
  cout << "STRING " << idx << ": ";
  while (true) {
    cout << *current_char;
    current_char++;
    if (current_char == this->source_end) {
      current_char = this->source;
    }
    if (current_char == starting_char) {
      break;
    }
  }
  cout << "\n";
}

/*
 * Poczatek:
 * 0    B A B A C A 3
 * 1    A B A C A B 0
 * 2    B A C A B A 4
 * 3    A C A B A B 1
 * 4    C A B A B A 5
 * 5    A B A B A C 2
 *
 * Iteracja 0:
 * 1    A B A C A B 3
 * 3    A C A B A B 5
 * 5    A B A B A C 4
 * 0    B A B A C A 0
 * 2    B A C A B A 1
 * 4    C A B A B A 2
 * 3 0 4 1 5 2
 *
 *
 * Iteracja 1a:
 * 0    B A B A C A 3
 * 2    B A C A B A 4
 * 4    C A B A B A 5
 * 1    A B A C A B 0
 * 5    A B A B A C 1
 * 3    A C A B A B 2
 * 0 3 1 5 2 4
 *
 *
 * Iteracja 1b:
 * 1    A B A C A B
 * 5    A B A B A C
 * 3    A C A B A B
 * 0    B A B A C A
 * 2    B A C A B A
 * 4    C A B A B A
 * 3 0 4 2 5 1
 */
