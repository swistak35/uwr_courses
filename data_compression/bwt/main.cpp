#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include "vitter.h"

#define TESTING_MODE false

void arc_put1 (unsigned bit);
unsigned arc_get1 ();
unsigned char ArcBit = 0;
int ArcChar = 0;

FILE * huffman_target;
FILE * huffman_source;

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

    huffman_target = fopen("huffman_target_file", "wb");

    HCoder * huff = huff_init(256, 256);
    for (int i = 0; i < source_len; i++) {
      huff_encode(huff, mtf_tbl[i]);
    }

    fclose(huffman_target);

    huffman_source = fopen("huffman_target_file", "wb");
    ArcBit = ArcChar = 0;
    HCoder * huff2 = huff_init(256, 256);
    for (int i = 0; i < source_len; i++) {
      mtf_tbl[i] = huff_decode(huff);
    }
    fclose(huffman_source);
    

    // DemoveToFront
    char target2[source_len + 1] = { 0 };
    DemoveToFront * demtf = new DemoveToFront(source_len);
    demtf->transform(mtf_tbl, target2);
    cout << "Target2: " << target2 << endl;
  }

  return 0;
}

void arc_put1 (unsigned bit)
{
    ArcChar <<= 1;

    if( bit )
        ArcChar |= 1;

    if( ++ArcBit < 8 )
        return;

    putc (ArcChar, huffman_target);
    ArcChar = ArcBit = 0;
}

unsigned arc_get1 ()
{
    if( !ArcBit )
        ArcChar = getc (huffman_source), ArcBit = 8;

    return ArcChar >> --ArcBit & 1;
}
