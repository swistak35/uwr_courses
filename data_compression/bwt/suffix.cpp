#include "SuffixBWT.h"
#include "LexiBWT.h"
#include <cstdio>
#include <strings.h>

using namespace std;

int main() {
  unsigned char txt[] = "ababbabbaabbabbz";
  int target1[17] = { 0 };
  SuffixBWT * suffix_bwt = new SuffixBWT(16);
  int res1 = suffix_bwt->transform(txt, target1);
  cout << "Target1: ";
  for (int i = 0; i < 16; i++) {
    cout << target1[i] << " ";
  }
  cout << endl;
  cout << "Res1: " << res1 << endl;

  unsigned char txt2[] = "ababbabbaabbabbz";
  int target2[17] = { 0 };
  LexiBWT * lexi_bwt = new LexiBWT(16);
  int res2 = lexi_bwt->transform(txt2, target2);
  cout << "Target2: ";
  for (int i = 0; i < 16; i++) {
    cout << target2[i] << " ";
  }
  cout << endl;
  cout << "Res2: " << res2 << endl;

  return 0;
}
