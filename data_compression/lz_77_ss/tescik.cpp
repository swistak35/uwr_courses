#include "SuffixBWT.h"

unsigned char txt[] = "ababbabbaabbabbz";
/* unsigned char txt[] = "cacaoz"; */

int size = sizeof(txt) - 1;

int main() {
  int target1[size + 1] = { 0 };
  SuffixTree * suffix_bwt = new SuffixTree(size, txt);
  suffix_tree->initialize(size);
  suffix_tree->insert_next();
  suffix_tree->insert_next();
  suffix_tree->insert_next();
  suffix_tree->insert_next();
  suffix_tree->print_tree();

  /* cout << "Target1: "; */
  /* for (int i = 0; i < size; i++) { */
  /*   cout << target1[i] << " "; */
  /* } */
  /* cout << endl; */
  /* cout << "Target1: "; */
  /* for (int i = 0; i < size; i++) { */
  /*   cout << (char) target1[i]; */
  /* } */
  return 0;
}
