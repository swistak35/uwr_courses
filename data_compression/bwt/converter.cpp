#include "LexiBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include <cassert>

using namespace std;

#define DEBUG 1

int main() {
  /* unsigned char txt[100] = { 0, 4, 4, 0, 0, 0, 1, 1, 1, 0, 0, 2, 2, 2, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 0, 0, 0, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 2, 0, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 0, 0, 0, 0, 4, 4, 4, 1, 0, 0, 0, 2, 2, 2, 3, 3, 3, 3, 1, 0, 1, 4, 4 }; */
  unsigned char txt[100] = { 2, 0, 0, 1, 0, 3, 1, 1, 1, 4, 0, 1, 1, 2, 3, 0, 4, 1, 2, 3, 0, 2, 3, 1, 0, 4, 0, 0, 3, 0, 0, 0, 1, 0, 3, 1, 4, 0, 2, 2, 1, 0, 0, 4, 2, 1, 0, 1, 2, 4, 1, 4, 1, 1, 1, 3, 0, 1, 3, 0, 2, 4, 2, 0, 1, 1, 1, 0, 1, 1, 0, 2, 3, 0, 3, 0, 3, 3, 3, 0, 4, 1, 2, 0, 3, 3, 0, 0, 1, 4, 1, 0, 0, 3, 0, 1, 4, 2, 3, 0};
  if (DEBUG) {
    cout << "TXT: ";
    for (int j = 0; j < 100; j++) {
      cout << " " << +txt[j];
    }
    cout << endl;
  }

  {
    int probs[5] = { 0 };
    for (int i = 0; i < 100; i++) {
      probs[txt[i]]++;
    }
    cout << "Probs: ";
    for (int i = 0; i < 5; i++) {
      cout << probs[i] << " ";
    }
    cout << endl;
  }

  /* unsigned char * ptr = source2; */
  /* for (int i = 0; i < 100; i++) { */
  /*   if (txt[i] == '0') { */
  /*     *ptr = 0; */
  /*   } else if (txt[i] == '1') { */
  /*     *ptr = 1; */
  /*   } else if (txt[i] == '2') { */
  /*     *ptr = 2; */
  /*   } else if (txt[i] == '3') { */
  /*     *ptr = 3; */
  /*   } else if (txt[i] == '4') { */
  /*     *ptr = 4; */
  /*   } */
  /*   ptr++; */
  /* } */

  assert(sizeof(txt) == 100);

  int target[100] = { 0 };
  LexiBWT * bwt = new LexiBWT(100);
  int orig_idx = bwt->transform(txt, target);
  if (DEBUG) {
    cout << "BWT1: ";
    for (int j = 0; j < 100; j++) {
      cout << " " << target[j];
    }
    cout << endl << "Index of orig string: " << orig_idx << endl;
  }
  {
    int probs[5] = { 0 };
    for (int i = 0; i < 100; i++) {
      probs[target[i]]++;
    }
    cout << "Probs: ";
    for (int i = 0; i < 5; i++) {
      cout << probs[i] << " ";
    }
    cout << endl;
  }

  MoveToFront * mtf = new MoveToFront();
  int mtf_tbl[100];
  mtf->target = mtf_tbl;
  mtf->run(target, 100);

  if (DEBUG) {
    cout << "MTF1:";
    for (int i = 0; i < 100; i++) {
      cout << " " << mtf_tbl[i];
    }
    cout << endl;
  }
  {
    int probs[5] = { 0 };
    for (int i = 0; i < 100; i++) {
      probs[mtf_tbl[i]]++;
    }
    cout << "Probs: ";
    for (int i = 0; i < 5; i++) {
      cout << probs[i] << " ";
    }
    cout << endl;
  }

  cout << "----------------------------------" << endl;

  int txt2[100] = { 2, 0, 0, 1, 0, 3, 1, 1, 1, 4, 0, 1, 1, 2, 3, 0, 4, 1, 2, 3, 0, 2, 3, 1, 0, 4, 0, 0, 3, 0, 0, 0, 1, 0, 3, 1, 4, 0, 2, 2, 1, 0, 0, 4, 2, 1, 0, 1, 2, 4, 1, 4, 1, 1, 1, 3, 0, 1, 3, 0, 2, 4, 2, 0, 1, 1, 1, 0, 1, 1, 0, 2, 3, 0, 3, 0, 3, 3, 3, 0, 4, 1, 2, 0, 3, 3, 0, 0, 1, 4, 1, 0, 0, 3, 0, 1, 4, 2, 3, 0};
  MoveToFront * mtf2 = new MoveToFront();
  int mtf_tbl2[100];
  mtf2->target = mtf_tbl2;
  mtf2->run(txt2, 100);

  if (DEBUG) {
    cout << "MTF:";
    for (int i = 0; i < 100; i++) {
      cout << " " << mtf_tbl2[i];
    }
    cout << endl;
  }
  int str_counts[5] = { 0 };
  int mtf_counts[5] = { 0 };
  for (int j = 0; j < 100; j++) {
    str_counts[txt2[j]]++;
    mtf_counts[mtf_tbl2[j]]++;
  }
  cout << "str_counts: ";
  for (int j = 0; j < 5; j++) {
    cout << str_counts[j] << " ";
  }
  cout << endl;
  cout << "mtf_counts: ";
  for (int j = 0; j < 5; j++) {
    cout << mtf_counts[j] << " ";
  }
  cout << endl;

  /* cout << "----------------------------------" << endl; */

  /* int txt3[100] = { 0, 4, 4, 0, 0, 0, 1, 1, 1, 0, 0, 2, 2, 2, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 0, 0, 0, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 2, 0, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 0, 0, 0, 0, 4, 4, 4, 1, 0, 0, 0, 2, 2, 2, 3, 3, 3, 3, 1, 0, 1, 4, 4 }; */
  
  /* DemoveToFront * demtf = new DemoveToFront(); */
  /* int target2[100] = { 0 }; */
  /* demtf->source = txt3; */
  /* demtf->run(target2, 100); */
  /* if (DEBUG) { */
  /*   cout << "BWT2: "; */
  /*   for (int j = 0; j < current_chunk_size; j++) { */
  /*     cout << " " << target2[j]; */
  /*   } */
  /*   cout << "\nBWT2 successful" << endl; */
  /* } */





  return 0;
}
