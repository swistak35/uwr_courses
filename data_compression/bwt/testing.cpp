#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include "Huffman.h"

using namespace std;

/* const char input_filename[] = "pantadeusz.txt"; */
/* const char output_bin_filename[] = "pantadeusz.bin"; */
/* const char output_orig_filename[] = "pantadeusz2.txt"; */
const char input_filename[] = "wiersz.txt";
const char compressed_filename[] = "wiersz.bin";
const char decompressed_filename[] = "wiersz2.txt";

int main() {
  // open source file
  FILE * source_file = fopen(input_filename, "rb");

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
  int mtf_tbl[source_len + 4];
  MoveToFront * mtf = new MoveToFront();
  mtf->target = mtf_tbl;
  mtf->run(orig_idx);
  mtf->run(target, source_len);

  cout << "MTF1:";
  for (int i = 0; i < source_len; i++) {
    cout << " " << mtf_tbl[i];
  }
  cout << endl;

  // Huffman
  FILE * htarget = fopen(compressed_filename, "wb");
  Huffman * huffman1 = new Huffman(1, 0);
  huffman1->data_in = mtf_tbl;
  huffman1->Out = htarget;
  huffman1->compress_init(source_len + 4);
  huffman1->compress();
  huffman1->compress_finish();
  fclose(htarget);

  // Dehuffman
  FILE * hsource = fopen(compressed_filename, "rb");
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
  char target2[source_len2 + 1 - 4] = { 0 };
  DemoveToFront * demtf = new DemoveToFront();
  demtf->source = mtf_tbl2;
  int orig_idx2;
  demtf->run(&orig_idx2);
  cout << "Orig idx2: " << orig_idx2 << endl;
  demtf->run(target2, source_len2 - 4);
  cout << "BWT2: " << target2 << endl;

  // Decoding
  char source2[source_len2 + 1 - 4] = { 0 };
  LexiDeBWT * debwt = new LexiDeBWT(source_len2 - 4);
  debwt->transform(orig_idx2, target2, source2);
  cout << "Source2: " << source2 << endl;

  FILE * decompressed_file = fopen(decompressed_filename, "wb");
  fwrite(source2, source_len2 - 4, 1, decompressed_file);
  fclose(decompressed_file);

  return 0;
}
