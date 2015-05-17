#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"
#include "DemoveToFront.h"
#include "vitter.h"

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

    FILE * hsource1 = fopen("wiersz.txt", "rb");
    FILE * htarget1 = fopen("bin_wiersz", "wb");
    Huffman * huffman1 = new Huffman(hsource1, htarget1);
    huffman1->compress();
    fclose(hsource1);
    fclose(htarget1);

    FILE * hsource2 = fopen("bin_wiersz", "rb");
    FILE * htarget2 = fopen("wiersz3.txt", "wb");
    Huffman * huffman2 = new Huffman(hsource2, htarget2);
    huffman2->decompress();
    fclose(hsource2);
    fclose(htarget2);

    

    // DemoveToFront
    char target2[source_len + 1] = { 0 };
    DemoveToFront * demtf = new DemoveToFront(source_len);
    demtf->transform(mtf_tbl, target2);
    cout << "Target2: " << target2 << endl;
  }

  return 0;
}
