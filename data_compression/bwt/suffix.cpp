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
  int res1 = suffix_bwt->transform(txt, target);
  printf("Target1: `%s` (%d)\n", target, res1);

  char target2[17];
  bzero(target2, sizeof(target2));
  LexiBWT * lexi_bwt = new LexiBWT(16);
  int res2 = lexi_bwt->transform(txt, target2);
  printf("Target2: `%s` (%d)\n", target2, res2);

  return 0;
}
