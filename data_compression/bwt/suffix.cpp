#include "SuffixBWT.h"

using namespace std;

int main() {
  char txt[] = "ababbabbaabbabbz";
  char target[17];
  SuffixBWT * suffix_bwt = new SuffixBWT(16);
  suffix_bwt->transform(txt, target);

  return 0;
}
