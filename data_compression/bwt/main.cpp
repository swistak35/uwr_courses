#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include "Huffman.h"

#define TESTING_MODE false

using namespace std;

int main() {

  if (TESTING_MODE) {
    // Init source
    char source[] = "babacabc";
    int source_len = strlen(source);
    std::cout << "Source: " << source << std::endl;

    // Coding
    char target[source_len + 1] = { 0 };
    LexiBWT * bwt = new LexiBWT(source_len);
    int orig_idx = bwt->transform(source, target);
    std::cout << "Target: " << target << std::endl;
    std::cout << "Index of orig string: " << orig_idx << std::endl;

    // Decoding
    char source2[source_len + 1] = { 0 };
    LexiDeBWT * debwt = new LexiDeBWT(source_len);
    debwt->transform(orig_idx, target, source2);
    std::cout << "Source2: " << source2 << std::endl;

    // MoveToFront
    int mtf_tbl[source_len];
    MoveToFront * mtf = new MoveToFront(source_len);
    mtf->transform(target, mtf_tbl);
    cout << "MTF:";
    for (int i = 0; i < source_len; i++) {
      cout << " " << mtf_tbl[i];
    }
    cout << endl;

    // DemoveToFront
    char target2[source_len + 1] = { 0 };
    DemoveToFront * demtf = new DemoveToFront(source_len);
    demtf->transform(mtf_tbl, target2);
    cout << "Target2: " << target2 << endl;
  } else {
    FILE * dane = fopen("wiersz.txt", "r");
    fseek(dane, 0, SEEK_END);
    int source_len = ftell(dane);
    rewind(dane);
    char * source = (char *) malloc(source_len + 1);
    source[source_len] = 0;
    fread(source, source_len, 1, dane);
    /* if (source[source_len - 1] == 0) { */
    /*   std::cout << "Istotnie jest 0!" << std::endl; */
    /* } else { */
    /*   std::cout << "Nie ma 0!" << std::endl; */
    /* } */
    fclose(dane);
    char * target = (char *) malloc(source_len + 1);
    target[source_len] = 0;
    LexiBWT * bwt = new LexiBWT(source_len);
    int orig_idx = bwt->transform(source, target);
    std::cout << "Target: " << target << std::endl;
    std::cout << "Index of orig string: " << orig_idx << std::endl;

    // Decoding
    char source2[source_len + 1] = { 0 };
    LexiDeBWT * debwt = new LexiDeBWT(source_len);
    debwt->transform(orig_idx, target, source2);
    std::cout << "Source2: " << source2 << std::endl;

    // MoveToFront
    int mtf_tbl[source_len];
    MoveToFront * mtf = new MoveToFront(source_len);
    mtf->transform(target, mtf_tbl);
    cout << "MTF:";
    for (int i = 0; i < source_len; i++) {
      cout << " " << mtf_tbl[i];
    }
    cout << endl;

    /* FILE * hsource1 = fopen("wiersz.txt", "rb"); */
    FILE * htarget1 = fopen("bin_wiersz", "wb");
    Huffman * huffman1 = new Huffman(1, 0);
    /* huffman1->In = hsource1; */
    huffman1->data_in = source;
    huffman1->Out = htarget1;
    huffman1->compress_init(source_len);
    for (int i = 0; i < source_len; i++) {
      huffman1->compress(source[i]);
    }
    huffman1->compress_finish();
    fclose(htarget1);

    FILE * hsource2 = fopen("bin_wiersz", "rb");
    bzero(target, source_len);
    Huffman * huffman2 = new Huffman(0, 1);
    huffman2->In = hsource2;
    huffman2->data_out = target;
    huffman2->decompress_init();
    /* for (int i = 0; i < source_len; i++) { */
      huffman2->decompress();
    /* } */
    cout << " Target huffman: " << target << endl;
    /* huffman2->decompress_finish(); */
    fclose(hsource2);

    /* FILE * In = fopen("wiersz.txt", "rb"); */
    /* FILE * Out = fopen("bin_wiersz", "wb"); */
    /* Huffman * huffman2 = new Huffman(0, 0); */
    /* huffman2->In = In; */
    /* huffman2->Out = Out; */
    /* unsigned int size = 256; */
    /* unsigned mask = ~0; */
    /* int symbol; */
    /* huffman2->huff_init (256, size); */
    /* putc (size >> 8, Out); */
    /* putc (size, Out); */

    /* fseek(In, 0, 2); */
    /* size = ftell(In); */
    /* fseek(In, 0, 0); */

    /* putc(size >> 16, Out); */
    /* putc(size >> 8, Out); */
    /* putc(size, Out); */

    /* while( size ) { */
    /*   if( symbol = getc(In), huffman2->huff_encode(symbol), size-- & mask ) { */
    /*     continue; */
    /*   } else { */
    /*     huffman2->huff_scale(1); */
    /*   } */
    /* } */

    /* while (huffman2->ArcBit) { */
    /*   huffman2->arc_put1(0); */
    /* } */

    /* fclose(In); */
    /* fclose(Out); */

    

    // DemoveToFront
    char target2[source_len + 1] = { 0 };
    DemoveToFront * demtf = new DemoveToFront(source_len);
    demtf->transform(mtf_tbl, target2);
    cout << "Target2: " << target2 << endl;
  }

  return 0;
}
