#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "SuffixBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include "Huffman.h"

using namespace std;

#define CHUNK_SIZE 256
#define DEBUG 0
#define PHASE_COMP 1
#define PHASE_DECOMP 1

void compress();
void decompress();

const char input_filename[] = "pantadeusz.txt";
const char compressed_filename[] = "pantadeusz.bin";
const char decompressed_filename[] = "pantadeusz2.txt";
/* const char input_filename[] = "wiersz.txt"; */
/* const char compressed_filename[] = "wiersz.bin"; */
/* const char decompressed_filename[] = "wiersz2.txt"; */

int main() {
  if (PHASE_COMP) {
    compress();
  }
  if (PHASE_DECOMP) {
    decompress();
  }

  return 0;
}

void compress() {
  // open source file
  FILE * source_file = fopen(input_filename, "rb");

  // calculate source file size
  fseek(source_file, 0, SEEK_END);
  int source_len = ftell(source_file);
  rewind(source_file);
  if (DEBUG) {
    cout << "Source len1: " << source_len << endl;
  }

  char source[CHUNK_SIZE + 1] = { 0 };
  int chunks, last_chunk_size;
  if (source_len % CHUNK_SIZE == 0) {
    chunks = source_len / CHUNK_SIZE;
    last_chunk_size = CHUNK_SIZE;
  } else {
    chunks = (source_len / CHUNK_SIZE) + 1;
    last_chunk_size = source_len % CHUNK_SIZE;
  }

  MoveToFront * mtf = new MoveToFront();
  int mtf_tbl[CHUNK_SIZE + 4];
  FILE * htarget = fopen(compressed_filename, "wb");
  Huffman * huffman1 = new Huffman(1, 0);
  huffman1->Out = htarget;
  huffman1->compress_init(source_len + 4 * chunks);
  for (int i = 0; i < chunks; i++) {
    cout << "Compressing " << float(i) / chunks * 100 << "%\n";
    int chunk_size;
    if (i == chunks-1) {
      chunk_size = last_chunk_size;
    } else {
      chunk_size = CHUNK_SIZE;
    }
    if (DEBUG) {
      cout << "Chunk size " << i << ": " << chunk_size << endl;
    }
    fread(source, chunk_size, 1, source_file);

    // run bwt
    char target[chunk_size + 1] = { 0 };
    LexiBWT * bwt = new LexiBWT(chunk_size);
    /* SuffixBWT * bwt = new SuffixBWT(chunk_size); */
    int orig_idx = bwt->transform(source, target);
    if (DEBUG) {
      cout << "BWT: " << target << endl;
      cout << "Index of orig string: " << orig_idx << endl;
    }

    // MoveToFront
    mtf->target = mtf_tbl;
    mtf->run(orig_idx);
    mtf->run(target, chunk_size);

    if (DEBUG) {
      cout << "MTF1:";
      for (int i = 0; i < chunk_size + 4; i++) {
        cout << " " << mtf_tbl[i];
      }
      cout << endl;
    }

    // Huffman
    huffman1->data_in = mtf_tbl;
    huffman1->compress(chunk_size + 4);
  }
  huffman1->compress_finish();
  fclose(htarget);
  fclose(source_file);
}

void decompress() {
  // Dehuffman
  FILE * hsource = fopen(compressed_filename, "rb");
  Huffman * huffman2 = new Huffman(0, 1);
  huffman2->In = hsource;
  int source_len2 = huffman2->decompress_init();
  int chunks2, last_chunk_size2;
  if (source_len2 % (CHUNK_SIZE + 4) == 0) {
    chunks2 = source_len2 / (CHUNK_SIZE + 4);
    last_chunk_size2 = CHUNK_SIZE;
  } else {
    chunks2 = (source_len2 / (CHUNK_SIZE + 4)) + 1;
    last_chunk_size2 = (source_len2 % (CHUNK_SIZE + 4)) - 4;
  }
  if (DEBUG) {
    cout << "Source len2: " << source_len2 << endl;
  }

  int mtf_tbl2[CHUNK_SIZE + 4];
  char target2[CHUNK_SIZE + 1 - 4] = { 0 };
  DemoveToFront * demtf = new DemoveToFront();
  FILE * decompressed_file = fopen(decompressed_filename, "wb");
  char source2[CHUNK_SIZE + 1 - 4] = { 0 };
  for (int i = 0; i < chunks2; i++) {
    cout << "Decompressing " << float(i) / chunks2 * 100 << "%\n";
    bzero(source2, CHUNK_SIZE + 1 - 4);
    int chunk_size;
    if (i == chunks2-1) {
      chunk_size = last_chunk_size2;
    } else {
      chunk_size = CHUNK_SIZE;
    }
    if (DEBUG) {
      cout << "Chunk size " << i << ": " << chunk_size << endl;
    }

    huffman2->data_out = mtf_tbl2;
    huffman2->decompress(chunk_size + 4);

    if (DEBUG) {
      cout << "MTF2:";
      for (int i = 0; i < chunk_size + 4; i++) {
        cout << " " << mtf_tbl2[i];
      }
      cout << endl;
    }

    demtf->source = mtf_tbl2;
    int orig_idx2;
    demtf->run(&orig_idx2);
    if (DEBUG) {
      cout << "Orig idx2: " << orig_idx2 << endl;
    }
    demtf->run(target2, chunk_size);
    if (DEBUG) {
      cout << "BWT2: " << target2 << endl;
      cout << "BWT2 successful" << endl;
    }

    // Decoding
    LexiDeBWT * debwt = new LexiDeBWT(chunk_size);
    debwt->transform(orig_idx2, target2, source2);
    if (DEBUG) {
      cout << "Source2: " << source2 << endl;
    }

    fwrite(source2, chunk_size, 1, decompressed_file);
  }
  fclose(hsource);
  fclose(decompressed_file);
}
