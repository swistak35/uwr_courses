#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include "Huffman.h"

using namespace std;

int main() {
  // open source file
  FILE * source_file = fopen("wiersz.txt", "rb");

  // calculate source file size
  fseek(source_file, 0, SEEK_END);
  int source_len = ftell(source_file);
  rewind(source_file);

  // copy file into memory
  char source[source_len + 1] = { 0 };
  fread(source, source_len, 1, source_file);
  fclose(source_file);

  // run bwt
  char target[source_len + 1] = { 0 };
  LexiBWT * bwt = new LexiBWT(source_len);
  int orig_idx = bwt->transform(source, target);
  cout << "BWT: " << target << endl;
  cout << "Index of orig string: " << orig_idx << endl;

  // MoveToFront
  int mtf_tbl[source_len];
  MoveToFront * mtf = new MoveToFront();
  mtf->target = mtf_tbl;
  mtf->run(target, source_len);

  cout << "MTF1:";
  for (int i = 0; i < source_len; i++) {
    cout << " " << mtf_tbl[i];
  }
  cout << endl;

  // Huffman
  FILE * htarget = fopen("wiersz.bin", "wb");
  Huffman * huffman1 = new Huffman(1, 0);
  huffman1->data_in = mtf_tbl;
  huffman1->Out = htarget;
  huffman1->compress_init(source_len);
  huffman1->compress();
  huffman1->compress_finish();
  fclose(htarget);

  // Dehuffman
  FILE * hsource = fopen("wiersz.bin", "rb");
  Huffman * huffman2 = new Huffman(0, 1);
  huffman2->In = hsource;
  int source_len2 = huffman2->decompress_init();
  cout << "Source len2: " << source_len2 << endl;
  int mtf_tbl2[source_len2];
  huffman2->data_out = mtf_tbl2;
  huffman2->decompress();
  fclose(hsource);
  cout << "MTF2:";
  for (int i = 0; i < source_len; i++) {
    cout << " " << mtf_tbl2[i];
  }
  cout << endl;

  // DemoveToFront
  char target2[source_len2 + 1] = { 0 };
  DemoveToFront * demtf = new DemoveToFront();
  demtf->source = mtf_tbl2;
  demtf->run(target2, source_len2);
  cout << "BWT2: " << target2 << endl;

  // Decoding
  char source2[source_len2 + 1] = { 0 };
  LexiDeBWT * debwt = new LexiDeBWT(source_len2);
  debwt->transform(orig_idx, target2, source2);
  cout << "Source2: " << source2 << endl;

  return 0;
}
