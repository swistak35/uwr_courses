#include <cstring>
#include <cstdio>
#include <cassert>
#include "SimpleSW.h"
#include "Huffman.h"

using namespace std;

#define DEFAULT_DICT_BUFFER 16
#define DEFAULT_INPUT_BUFFER 4

void print_buf(unsigned char * buf, int start, int size);
int upper_bound(int x, int bound);
int wrap(int x, int size);

int main(int argc, char ** argv) {
  char * input_filename;
  char * output_filename;
  int max_dict_buf_size = DEFAULT_DICT_BUFFER;
  int max_input_buf_size = DEFAULT_INPUT_BUFFER;

  if (argc >= 3) {
    input_filename  = argv[1];
    output_filename = argv[2];
    if (argc >= 5) {
      max_dict_buf_size = atoi(argv[3]);
      max_input_buf_size = atoi(argv[4]);
    }
  } else {
    printf("Złe argumenty\n");
    printf("./compress-lz77 plik_wejsciowy plik_wyjsciowy bufor_slownika=256 bufor_wejsciowy=32\n");
    exit(EXIT_FAILURE);
  }

  // open source file
  FILE * input_file = fopen(input_filename, "rb");
  if (input_file == NULL) {
    printf("Plik zrodlowy nie istnieje\n");
    exit(EXIT_FAILURE);
  }

  // calculate source file size
  fseek(input_file, 0, SEEK_END);
  int source_len = ftell(input_file);
  rewind(input_file);
#ifdef DEBUG
  printf("== Source length: %d\n", source_len);
#endif

  int remaining_bytes = source_len;
  unsigned char dict_buf[max_dict_buf_size];
  unsigned char input_buf[max_input_buf_size];
  unsigned char following_char_buf[1];
  int dict_buf_idx = 0, dict_buf_size = 0;
  int input_buf_idx = 0, input_buf_size = max_input_buf_size;
  int following_char;
  WindowResult * result = (WindowResult *) malloc(sizeof(WindowResult));
  if (result == NULL) {
    printf("WindowResult malloc failure.");
    exit(EXIT_FAILURE);
  }
  SimpleSW * window = new SimpleSW(result);
  window->set_dict_buf(dict_buf, &dict_buf_idx, &dict_buf_size, max_dict_buf_size);
  window->set_input_buf(input_buf, &input_buf_idx, &input_buf_size, max_input_buf_size);

  result->length = max_input_buf_size;
  while (remaining_bytes > 0) {
    // reading input
    printf("==compress-lz77== Remaining bytes: %d\n", remaining_bytes);
    if (result->length == max_input_buf_size) {
      fread(input_buf, max_input_buf_size, 1, input_file);
      input_buf_idx = 0;
    } else {
      int prev_input_buf_idx = wrap(input_buf_idx - result->length, max_input_buf_size);
      if (input_buf_idx - result->length >= 0) {
        fread(input_buf + prev_input_buf_idx, result->length + 1, 1, input_file);
      } else {
        fread(input_buf + prev_input_buf_idx, max_input_buf_size - prev_input_buf_idx, 1, input_file);
        fread(input_buf, input_buf_idx, 1, input_file);
        assert(max_input_buf_size - prev_input_buf_idx + input_buf_idx == result->length + 1);
      }
    }
    printf("==compress-lz77== Dict: "); print_buf(dict_buf, dict_buf_idx, dict_buf_size);
    printf("==compress-lz77== Input: "); print_buf(input_buf, input_buf_idx, input_buf_size);

    // step
    printf("==compress-lz77== before step: input_buf_idx=%d dict_buf_idx=%d dict_buf_size=%d\n", input_buf_idx, dict_buf_idx, dict_buf_size);
    window->step();
    printf("==compress-lz77== after step: input_buf_idx=%d dict_buf_idx=%d dict_buf_size=%d\n", input_buf_idx, dict_buf_idx, dict_buf_size);

    // following char
    if (result->length == input_buf_size) {
      fread(following_char_buf, 1, 1, input_file);
      following_char = following_char_buf[0];
      window->insert_char_into_dict(following_char_buf[0]);
    } else {
      following_char = input_buf[input_buf_idx];
      window->insert_char_into_dict(input_buf[input_buf_idx]);
      input_buf_idx++;
    }
    // byc moze po wczytaniu calego inputu, tutaj bedzie poprzedni juz wczytany znak, trzeba wczytywac jakos wiecej
    printf("==compress-lz77== after insert: input_buf_idx=%d dict_buf_idx=%d dict_buf_size=%d\n", input_buf_idx, dict_buf_idx, dict_buf_size);

    // finish
    printf("(%d, %d, %d)|\n", result->offset, result->length, following_char);
    remaining_bytes -= result->length + 1;
    /* prev_input_buf_idx = input_buf_idx; */
  }

  /* MoveToFront * mtf = new MoveToFront(); */
  /* int mtf_tbl[max_chunk_size + 5]; */
  FILE * htarget = fopen(output_filename, "wb");
  /* Huffman * huffman1 = new Huffman(1, 0, 256); */
  /* huffman1->Out = htarget; */
  /* huffman1->compress_init(source_len + 5 * chunks); */
  /* for (int i = 0; i < chunks; i++) { */
  /*   cout << "Compressing " << float(i) / (chunks - 1) * 100 << "%\n"; */
  /*   int current_chunk_size; */
  /*   if (i == chunks-1) { */
  /*     current_chunk_size = last_chunk_size; */
  /*   } else { */
  /*     current_chunk_size = max_chunk_size; */
  /*   } */
  /*   if (DEBUG) { */
  /*     cout << "Chunk size " << i << ": " << current_chunk_size << endl; */
  /*   } */
    /* fread(source, current_chunk_size, 1, input_file); */
  /*   source[current_chunk_size] = 0; */

  /*   if (DEBUG) { */
  /*     cout << "Source: "; */
  /*     for (int j = 0; j < current_chunk_size + 1; j++) { */
  /*       cout << " " << +source[j]; */
  /*     } */
  /*     cout << endl; */
  /*   } */

  /*   // run bwt */
  /*   int target[current_chunk_size + 1] = { 0 }; */
  /*   /1* cout << "uuu: " << target[0]; *1/ */
  /*   cbeg = clock(); */
  /*   SuffixBWT * bwt = new SuffixBWT(current_chunk_size + 1); */
  /*   int orig_idx = bwt->transform(source, target); */
  /*   delete bwt; */
  /*   cend = clock(); */
  /*   cbwt += (cend - cbeg); */
  /*   if (DEBUG) { */
  /*     cout << "BWT: "; */
  /*     /1* cout << " " << target[0]; *1/ */
  /*     for (int j = 0; j < current_chunk_size + 1; j++) { */
  /*       cout << " " << target[j]; */
  /*     } */
  /*     cout << "\nBWTsuccessful" << endl; */
  /*     cout << "Index of orig string: " << orig_idx << endl; */
  /*   } */

  /*   // MoveToFront */
  /*   mtf->target = mtf_tbl; */
  /*   cbeg = clock(); */
  /*   mtf->run(orig_idx); */
  /*   mtf->run(target, current_chunk_size + 1); */
  /*   cend = clock(); */
  /*   cmtf += (cend - cbeg); */

  /*   if (DEBUG) { */
  /*     cout << "MTF:"; */
  /*     for (int i = 0; i < current_chunk_size + 4 + 1; i++) { */
  /*       cout << " " << mtf_tbl[i]; */
  /*     } */
  /*     cout << endl; */
  /*   } */

  /*   // Huffman */
  /*   huffman1->data_in = mtf_tbl; */
  /*   cbeg = clock(); */
  /*   huffman1->compress(current_chunk_size + 5); */
  /*   cend = clock(); */
  /*   chuff += (cend - cbeg); */
  /* } */
  /* huffman1->compress_finish(); */
  fclose(htarget);
  fclose(input_file);

  /* delete mtf; */
  /* delete huffman1; */

  /* printf("BWT: %ld | MTF: %ld | HUFF: %ld\n", cbwt, cmtf, chuff); */

  return 0;
}

void print_buf(unsigned char * buf, int start, int size) {
  int i = start;
  while (i < size) {
    printf("%d ", buf[i]);
    i++;
  }
  i = 0;
  while (i < start) {
    printf("%d ", buf[i]);
    i++;
  }
  printf("\n");
}

int upper_bound(int x, int bound) {
  if (x > bound) {
    return bound;
  } else {
    return x;
  }
}

int wrap(int x, int size) {
  if (x >= size) {
    return (x - size);
  } else if (x < 0) {
    return (x + size);
  } else {
    return x;
  }
}
