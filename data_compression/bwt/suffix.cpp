#include "SuffixBWT.h"
#include "LexiBWT.h"
#include <cstdio>
#include <strings.h>

using namespace std;

int main() {
  char txt[] = "ababbabbaabbabbz";

  char target[17];
  bzero(target, sizeof(target));
  SuffixBWT * suffix_bwt = new SuffixBWT(16);
  suffix_bwt->transform(txt, target);
  printf("Target1: `%s`\n", target);

  char target2[17];
  bzero(target2, sizeof(target2));
  LexiBWT * lexi_bwt = new LexiBWT(16);
  lexi_bwt->transform(txt, target2);
  printf("Target2: `%s`\n", target2);

  return 0;
}
