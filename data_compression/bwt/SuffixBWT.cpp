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

#define LIMITING_STEPS 100

void SuffixBWT::sort() {
  // init
  char * current_char = this->source;
  char * looking_char;
  BranchNode * root_node = create_branch_node();
  root_node->longestProperSuffix = root_node;
  root_node->depth = 0;
  BranchNode * active_node = root_node;
  int active_length = 0;
  int min_distance = 0;
  BranchNode * nodes[this->length + 1];
  // are they all null?
  
  Edge * next_edge, * edge_to_previous_target;
  BranchNode * new_bnode;
  BranchNode * last_added_node;
  for (int i = 0; i < this->length && i < LIMITING_STEPS; i++) {
    printf("======================================= %d\n", i);
    printf("=== %s\n", this->source + i);

    Edge * edge_to_suffix = create_edge();

    // podazaj za longest suffix
    printf("== Zaczynamy od active_node == %c\n", active_node->debugchar);
    active_node = active_node->longestProperSuffix; 
    active_length = active_node->depth;
    printf("== Przeszliśmy do active_node == %c\n", active_node->debugchar);

    bool found = false;
    while (!found) {
      looking_char = current_char + active_length;
      printf("== Ehhh %d, %d, %d\n", current_char - this->source, looking_char - this->source, active_length);
      next_edge = find_edge_on_list(active_node, *looking_char);

      if (next_edge == NULL) {
        printf("== Nie znalazlem nastepnej litery (%c), wiec dodaje nowa krawedz do %c\n", *looking_char, active_node);
        found = true;
        edge_to_suffix->digit = (int) *looking_char;
        edge_to_suffix->startingChar = active_length + i;
        edge_to_suffix->endingChar = this->length - 1;
        active_node->edges->push_back(edge_to_suffix);
      } else {
        /* looking_char++; */
        int k = 1;
        char * other_char = this->source + next_edge->startingChar;
        while (next_edge->startingChar + k <= next_edge->endingChar) {
          other_char++;
          looking_char++;
          printf("== Porownuje %c (%d) z %c (%d)\n", *other_char, other_char - this->source, *looking_char, looking_char - this->source);
          if (*other_char != *looking_char) {
            found = true;
            break;
          }
          k++;
        }

        if (found) {
          edge_to_previous_target = create_edge();
          edge_to_previous_target->digit = (int) *other_char;
          edge_to_previous_target->target = next_edge->target;
          edge_to_previous_target->startingChar = next_edge->startingChar + k;
          edge_to_previous_target->endingChar = next_edge->endingChar;
          edge_to_suffix->digit = (int) *looking_char;
          edge_to_suffix->startingChar = i + active_length + k;
          edge_to_suffix->endingChar = this->length - 1;
          new_bnode = create_branch_node();
          insert_edge_into_bnode(new_bnode, edge_to_previous_target);
          insert_edge_into_bnode(new_bnode, edge_to_suffix);
          /* new_bnode->edges.push_back(edge_to_previous_target); */
          /* new_bnode->edges.push_back(edge_to_suffix); */
          /* new_bnode->longestProperSuffix = root_node; */
          new_bnode->depth = active_node->depth + k;
          next_edge->endingChar = next_edge->startingChar + k - 1;
          next_edge->target = new_bnode;

          if (active_node == root_node && next_edge->endingChar == next_edge->startingChar) {
            new_bnode->longestProperSuffix = root_node;
            printf("== Ustawiam lps1 wierzch %c na %c\n", new_bnode->debugchar, root_node->debugchar);
          }
          if (i > 2 && last_added_node->longestProperSuffix == NULL) {
            last_added_node->longestProperSuffix = new_bnode;
            printf("== Ustawiam lps2 wierzch %c na %c\n", last_added_node->debugchar, new_bnode->debugchar);
          }
          /* if (i > 2) { */
          /*   printf("++ %c %d %d %d %d %d %d\n", */
          /*       last_added_node->debugchar, */
          /*       active_node == root_node, */
          /*       next_edge->endingChar == next_edge->startingChar, */
          /*       i > 2, */
          /*       last_added_node->longestProperSuffix == NULL, */
          /*       active_node == root_node && next_edge->endingChar == next_edge->startingChar, */
          /*       i > 2 && last_added_node->longestProperSuffix == NULL); */
          /* } */

          last_added_node = new_bnode;
        } else {
          active_node = next_edge->target;
          active_length = active_node->depth;
        }
      }
    }

    // utworz information node dla tego suffix
    nodes[i] = create_branch_node();
    nodes[i]->suffix_id = i;
    nodes[i]->depth = this->length - 1;

    // utworz krawedz dla nastepnego information node
    edge_to_suffix->target = nodes[i];

    // ...

    current_char++;
    print_tree(root_node);
  }

  /* set_ranks(root_node); */
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

void SuffixBWT::insert_edge_into_bnode(BranchNode * bnode, Edge * edge) {
  for (std::list<Edge*>::iterator it = bnode->edges->begin(); it != bnode->edges->end(); it++) {
    if (edge->digit < (*it)->digit) {
      bnode->edges->insert(it, edge);
      return;
    }
  }
  bnode->edges->push_back(edge);
}

void SuffixBWT::print_tree(BranchNode * node) {
  print_node(1, node);
}

char current_debug_char = 'A';

void SuffixBWT::print_node(int depth, BranchNode * node) {
  print_tabs(depth);
  if (node->longestProperSuffix == NULL) {
    printf("BranchNode< %c > (%d, %d) [ nil ]\n",
        node->debugchar,
        node->depth,
        node->suffix_id);
  } else {
    printf("BranchNode< %c > (%d, %d) [ %c ]\n",
        node->debugchar,
        node->depth,
        node->suffix_id,
        node->longestProperSuffix->debugchar);
  }
  Edge * edge;
  for (list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = *it;
    char str[32] = {0};
    int k = 0;
    for (int j = edge->startingChar; j <= edge->endingChar; j++) {
      str[k] = this->source[j];
      k++;
    }
    print_tabs(depth);
    printf("Edge ->%c (%d) [%d .. %d] <%s>\n",
        edge->target->debugchar,
        edge->digit,
        edge->startingChar, edge->endingChar,
        str);
  }
  for (list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = *it;
    print_node(depth + 1, edge->target);
  }
}

Edge * SuffixBWT::find_edge_on_list(BranchNode * node, int c) {
  printf("== edges size: %d %p %p %d\n", node->edges->empty(), node->edges->begin(), node->edges->end(), c);
  printf("== huehue: %d\n", node->edges->size());
  for (list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); ++it) {
    printf("== TRolololo\n");
    if ((*it)->digit == c) {
      return *it;
    }
  }
  return NULL;
}

// poinicjalizowac na 0, null itp.
BranchNode * SuffixBWT::create_branch_node() {
  BranchNode * ptr = (BranchNode *) calloc(1, sizeof(BranchNode));
  ptr->edges = new list<Edge*>();
  printf("CO jest kurwa %d\n", ptr->edges->empty());
  /* bzero(ptr, sizeof(BranchNode)); */
  ptr->longestProperSuffix = NULL;
  ptr->debugchar = current_debug_char;
  current_debug_char++;
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
