#include "SuffixBWT.h"

/* unsigned char txt[] = "ababbabbaabbabbz"; */
/* unsigned char txt[] = "cacaoz"; */

int size = sizeof(txt) - 1;

int main() {
  int target1[size + 1] = { 0 };
  SuffixBWT * suffix_bwt = new SuffixBWT(size);
  suffix_bwt->prepare();
  suffix_bwt->insert_next();

  cout << "Target1: ";
  for (int i = 0; i < size; i++) {
    cout << target1[i] << " ";
  }
  cout << endl;
  cout << "Target1: ";
  for (int i = 0; i < size; i++) {
    cout << (char) target1[i];
  }
  return 0;
}
