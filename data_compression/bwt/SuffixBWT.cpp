#include "SuffixBWT.h"
#include <iostream>
#include <assert.h>
#include <cstdio>
#include <strings.h>

using namespace std;

SuffixBWT::SuffixBWT(int length) {
  this->length = length;
  this->current_position = 0;
}

SuffixBWT::~SuffixBWT() {
  destroy_structures(this->root_node);
}

void SuffixBWT::destroy_structures(BranchNode * node) {
  Edge * edge;
  for (std::list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = *it;
    destroy_structures(edge->target);
    free(edge);
  }
  delete node->edges;
  free(node);
}

int SuffixBWT::transform(unsigned char * source, int * target) {
  this->source_end = source + (this->length - 1);
  this->source = source;
  this->target = target;

  this->ranks.clear();
  for (int i = 0; i < this->length; i++) {
    this->ranks.push_back(-1);
  }

  sort();

  set_ranks_root();

  /* return 0; */
  return this->ranks[0];
}

#define LIMITING_STEPS 3

void SuffixBWT::set_ranks_root() {
  set_ranks(0, this->root_node);
}

int SuffixBWT::update(BranchNode * node, int startingChar, int endingChar, BranchNode ** result) {
  BranchNode * oldr = this->root_node;
  BranchNode * information_node = NULL;
  BranchNode * bnode = NULL;
  int current_char = get_digit(this->source + endingChar);
  bool end_point = test_and_split(node, startingChar, endingChar - 1, current_char, &bnode);
  Edge * edge = NULL;

  while (!end_point) {
    information_node = create_branch_node();

    edge = create_edge();
    edge->target = information_node;
    edge->startingChar = endingChar;
    edge->endingChar = this->length - 1;
    edge->digit = get_digit(this->source + endingChar);
    insert_edge_into_bnode(bnode, edge);

    if (oldr != this->root_node) {
      oldr->longestProperSuffix = bnode;
    }
    oldr = bnode;
    startingChar = canonize(node->longestProperSuffix, startingChar, endingChar - 1, &node);
    end_point = test_and_split(node, startingChar, endingChar - 1, current_char, &bnode);
  }

  if (oldr != this->root_node) {
    oldr->longestProperSuffix = node;
  }

  *result = node;
  return startingChar;
}

bool SuffixBWT::test_and_split(BranchNode * node, int startingChar, int endingChar, int current_char,
    BranchNode ** bnode) {
  if (SUFFIX_BWT_VERBOSE) {
    printf("== test_and_split node=%d startingChar=%d endingChar=%d current_char=%d\n",
        node->debugchar, startingChar, endingChar, current_char);
  }
  if (startingChar <= endingChar) { // or endingChar == INFINITY
    int charAtStartingChar = get_digit(this->source + startingChar);
    Edge * edge = find_edge_on_list(node, charAtStartingChar);
    assert(edge != NULL);
    int forward_char = get_digit(this->source + edge->startingChar + endingChar - startingChar + 1);
    if (current_char == forward_char) {
      *bnode = node;
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
    Edge * edge = find_edge_on_list(node, current_char);
    *bnode = node;
    if (edge == NULL) {
      return false;
    } else {
      return true;
    }
  }
}

int SuffixBWT::canonize(BranchNode * node, int startingChar, int endingChar, BranchNode ** result) {
  if (endingChar < startingChar) { // pamietac o nieskonczonosci
    *result = node;
    return startingChar;
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

    *result = node;
    return startingChar;
  }
}


void SuffixBWT::sort() {
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

  BranchNode * current_node = root_node;
  BranchNode * result = NULL;
  int startingChar = 0;
  int current_char = -1;
  while (current_char != this->length - 1) {
    current_char++;
    if (SUFFIX_BWT_VERBOSE) {
      cout << "==============================================================" << endl;
      cout << "== Zaczynamy z " << current_char << endl;
      cout << "== Starting char " << startingChar << " | Current node: " << current_node->debugchar << endl;
    }
    startingChar = update(current_node, startingChar, current_char, &result);
    current_node = result;
    startingChar = canonize(current_node, startingChar, current_char, &result);
    current_node = result;

    if (SUFFIX_BWT_VERBOSE) {
      print_tree(root_node);
    }
  }
}

int SuffixBWT::get_digit(unsigned char * chr_ptr) {
  /* printf("get_digit %p %p\n", chr_ptr, this->source_end); */
  if (chr_ptr == this->source_end) {
    return 256;
  } else {
    return ((int) *chr_ptr);
  }
}


void SuffixBWT::set_ranks(int depth, BranchNode * node) {
  if (SUFFIX_BWT_VERBOSE) {
    printf("Setting ranks for node %d depth=%d...\n", node->debugchar, depth);
    printf("BranchNode< %d > (%d, %d) [ nil ]\n",
        node->debugchar,
        node->depth,
        node->suffix_id);
  }
  if (node->edges->size() == 0) {
    int suffix_id = this->length - depth;
    assert(suffix_id >= 0);
    assert(suffix_id < this->length);
    this->ranks[suffix_id] = this->current_position;
    /* printf("Current position: %d\n", this->current_position); */
    assert(current_position < this->length);
    /* this->target[current_position] = this->source[node->suffix_id]; */
    /* this->target[this->ranks[node->suffix_id]] = this->source[node->suffix_id]; */
    if (suffix_id == 0) {
      this->target[this->ranks[suffix_id]] = 0;
    } else {
      this->target[this->ranks[suffix_id]] = this->source[suffix_id - 1];
    }
    current_position++;
  }
  Edge * edge;
  for (std::list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = *it;
    set_ranks(depth + edge->endingChar - edge->startingChar + 1, edge->target);
  }
}

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

/* char current_debug_char = 'A'; */
int current_debug_char = 0;

void SuffixBWT::print_node(int depth, BranchNode * node) {
  print_tabs(depth);
  if (node->longestProperSuffix == NULL) {
    printf("BranchNode< %d > (%d, %d) [ nil ]\n",
        node->debugchar,
        node->depth,
        node->suffix_id);
  } else {
    printf("BranchNode< %d > (%d, %d) [ %d ]\n",
        node->debugchar,
        node->depth,
        node->suffix_id,
        node->longestProperSuffix->debugchar);
  }
  Edge * edge;
  for (list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = *it;
    /* char str[2048] = {0}; */
    /* int k = 0; */
    /* for (int j = edge->startingChar; j <= edge->endingChar; j++) { */
      /* str[k] = (char) this->source[j]; */
      /* k++; */
    /* } */
    print_tabs(depth);
    assert(edge->digit >= 0);
    assert(edge->digit <= 256);
    printf("Edge ->%d (%d) [%d .. %d] <>\n",
        edge->target->debugchar,
        edge->digit,
        edge->startingChar, edge->endingChar);
        /* edge->startingChar, edge->endingChar, */
        /* str); */
  }
  for (list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); it++) {
    edge = *it;
    print_node(depth + 1, edge->target);
  }
}

Edge * SuffixBWT::find_edge_on_list(BranchNode * node, int c) {
  for (list<Edge*>::iterator it = node->edges->begin(); it != node->edges->end(); ++it) {
    /* Edge * edge = *it; */
    /* int digit = (*it)->digit; */
    /* printf("Szukanie przez %p\n", edge); */
    /* printf("Szukanie cyfry %d\n", digit); */
    /* printf("A porownujemy do %d\n", c); */

    if ((*it)->digit == c) {
      return *it;
    }
  }
  return NULL;
}

// poinicjalizowac na 0, null itp.
BranchNode * SuffixBWT::create_branch_node() {
  BranchNode * ptr = (BranchNode *) malloc(sizeof(BranchNode));
  assert(ptr != NULL);
  ptr->suffix_id = -1;
  ptr->parent = NULL;
  ptr->edges = new list<Edge*>();
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
  Edge * ptr = (Edge *) malloc(sizeof(Edge));
  assert(ptr != NULL);
  ptr->digit = -99;
  return ptr;
}

int SuffixBWT::get_char_idx(int idx) {
  if (idx >= this->length) {
    return (idx - this->length);
  } else {
    return idx;
  }
}
  // init
  /* unsigned char * current_char = this->source; */
  /* unsigned char * looking_char = NULL; */
  /* BranchNode * root_node = create_branch_node(); */
  /* this->root_node = root_node; */
  /* root_node->longestProperSuffix = root_node; */
  /* root_node->depth = 0; */
  /* BranchNode * active_node = root_node; */
  /* int active_length = 0; */
  /* /1* int min_distance = 0; *1/ */
  /* BranchNode * nodes[this->length + 1]; */
  /* // are they all null? */
  
  /* Edge * next_edge = NULL; */
  /* Edge * edge_to_previous_target = NULL; */
  /* BranchNode * new_bnode = NULL; */
  /* BranchNode * last_added_node = NULL; */
  /* for (int i = 0; i < this->length && i < LIMITING_STEPS; i++) { */
  /*   if (SUFFIX_BWT_VERBOSE) { */
  /*     printf("======================================= %d\n", i); */
  /*     printf("=== %s\n", this->source + i); */
  /*   } */

  /*   Edge * edge_to_suffix = create_edge(); */

  /*   // podazaj za longest suffix */
  /*   if (SUFFIX_BWT_VERBOSE) { */
  /*     printf("== Zaczynamy od active_node == %d\n", active_node->debugchar); */
  /*   } */
  /*   if (active_node->longestProperSuffix != NULL) { */
  /*     active_node = active_node->longestProperSuffix; */ 
  /*     active_length = active_node->depth; */
  /*     if (SUFFIX_BWT_VERBOSE) { */
  /*       printf("== Przeszliśmy do active_node == %d\n", active_node->debugchar); */
  /*     } */
  /*   } */

  /*   bool found = false; */
  /*   while (!found) { */
  /*     looking_char = current_char + active_length; */
  /*     if (SUFFIX_BWT_VERBOSE) { */
  /*       printf("== Ehhh %p, %p, %d\n", current_char, looking_char, active_length); */
  /*     } */
  /*     next_edge = find_edge_on_list(active_node, *looking_char); */

  /*     if (next_edge == NULL) { */
  /*       if (SUFFIX_BWT_VERBOSE) { */
  /*         printf("== Nie znalazlem nastepnej litery (%c), wiec dodaje nowa krawedz do %c\n", *looking_char, active_node->debugchar); */
  /*       } */
  /*       found = true; */
  /*       edge_to_suffix->digit = get_digit(looking_char); */
  /*       edge_to_suffix->startingChar = active_length + i; */
  /*       edge_to_suffix->endingChar = this->length - 1; */
  /*       insert_edge_into_bnode(active_node, edge_to_suffix); */
  /*       nodes[i] = create_branch_node(); */
  /*       nodes[i]->parent = active_node; */

  /*       if (last_added_node != NULL) { */
  /*         assert(last_added_node->longestProperSuffix == NULL); */
  /*         BranchNode * candidate_node = nodes[i]; */
  /*         /1* print_node(0, last_added_node); *1/ */
  /*         printf("Ustalamy longestProperSuffix dla %d, zaczynamy od %d\n", last_added_node->debugchar, */
  /*             nodes[i]->debugchar); */
  /*         while (candidate_node->depth != last_added_node->depth - 1) { */
  /*           candidate_node = candidate_node->parent; */
  /*           assert(candidate_node != NULL); */
  /*           printf("Zamiana candidate_node na %d\n", candidate_node->debugchar); */
  /*         } */
  /*         last_added_node->longestProperSuffix = candidate_node; */
  /*       } */

  /*       last_added_node = NULL; */
  /*     } else { */
  /*       /1* looking_char++; *1/ */
  /*       int k = 1; */
  /*       unsigned char * other_char = this->source + next_edge->startingChar; */
  /*       while (next_edge->startingChar + k <= next_edge->endingChar) { */
  /*         other_char++; */
  /*         looking_char++; */
  /*         if (SUFFIX_BWT_VERBOSE) { */
  /*           printf("== Porownuje %c (%p) z %c (%p)\n", *other_char, other_char, *looking_char, looking_char); */
  /*         } */
  /*         if (*other_char != *looking_char) { */
  /*           found = true; */
  /*           break; */
  /*         } */
  /*         k++; */
  /*       } */

  /*       if (found) { */
  /*         edge_to_previous_target = create_edge(); */
  /*         edge_to_previous_target->digit = get_digit(other_char); */
  /*         edge_to_previous_target->target = next_edge->target; */
  /*         edge_to_previous_target->startingChar = next_edge->startingChar + k; */
  /*         edge_to_previous_target->endingChar = next_edge->endingChar; */
  /*         edge_to_suffix->digit = get_digit(looking_char); */
  /*         edge_to_suffix->startingChar = i + active_length + k; */
  /*         edge_to_suffix->endingChar = this->length - 1; */
  /*         new_bnode = create_branch_node(); */
  /*         insert_edge_into_bnode(new_bnode, edge_to_previous_target); */
  /*         insert_edge_into_bnode(new_bnode, edge_to_suffix); */
  /*         new_bnode->depth = active_node->depth + k; */
  /*         new_bnode->parent = active_node; */
  /*         next_edge->endingChar = next_edge->startingChar + k - 1; */
  /*         next_edge->target = new_bnode; */

  /*         if (last_added_node != NULL) { */
  /*           assert(last_added_node->longestProperSuffix == NULL); */
  /*           BranchNode * candidate_node = new_bnode; */
  /*           /1* print_node(0, last_added_node); *1/ */
  /*           printf("Ustalamy longestProperSuffix dla %d, zaczynamy od %d\n", last_added_node->debugchar, */
  /*               new_bnode->debugchar); */
  /*           while (candidate_node->depth != last_added_node->depth - 1) { */
  /*             candidate_node = candidate_node->parent; */
  /*             assert(candidate_node != NULL); */
  /*             printf("Zamiana candidate_node na %d\n", candidate_node->debugchar); */
  /*           } */
  /*           last_added_node->longestProperSuffix = candidate_node; */
  /*         } */

  /*         last_added_node = new_bnode; */
  /*         if (SUFFIX_BWT_VERBOSE) { */
  /*           printf("Ustawiamy last_added_node na %d\n", new_bnode->debugchar); */
  /*         } */
  /*         nodes[i] = create_branch_node(); */
  /*         nodes[i]->parent = new_bnode; */
  /*       } else { */
  /*         assert(next_edge->target != NULL); */
  /*         active_node = next_edge->target; */
  /*         active_length = active_node->depth; */
  /*       } */
  /*     } */
  /*   } */

  /*   // utworz information node dla tego suffix */
  /*   nodes[i]->suffix_id = i; */
  /*   nodes[i]->depth = this->length - 1; */
  /*   nodes[i]->longestProperSuffix = NULL; */

  /*   // utworz krawedz dla nastepnego information node */
  /*   edge_to_suffix->target = nodes[i]; */

  /*   // ... */

  /*   current_char++; */
  /*   if (SUFFIX_BWT_VERBOSE) { */
  /*     print_tree(root_node); */
  /*   } */
  /* } */

  /* set_ranks(root_node); */
  
  /* printf("%d", target[0]); */
