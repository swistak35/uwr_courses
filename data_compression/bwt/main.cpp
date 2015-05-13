#include <iostream>
#include <cstring>
#include <cstdio>
#include "LexiBWT.h"
#include "LexiDeBWT.h"
#include "MoveToFront.h"

#define TESTING_MODE true

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
  } else {
    FILE * dane = fopen("wiersz.txt", "r");
    fseek(dane, 0, SEEK_END);
    int dane_size = ftell(dane);
    rewind(dane);
    char * source = (char *) malloc(dane_size + 1);
    source[dane_size] = 0;
    fread(source, dane_size, 1, dane);
    /* if (source[dane_size - 1] == 0) { */
    /*   std::cout << "Istotnie jest 0!" << std::endl; */
    /* } else { */
    /*   std::cout << "Nie ma 0!" << std::endl; */
    /* } */
    fclose(dane);
    char * target = (char *) malloc(dane_size + 1);
    target[dane_size] = 0;
    LexiBWT * bwt = new LexiBWT(dane_size);
    int orig_idx = bwt->transform(source, target);
    std::cout << "Target: " << target << std::endl;
    std::cout << "Index of orig string: " << orig_idx << std::endl;

    // Decoding
    char source2[dane_size + 1] = { 0 };
    LexiDeBWT * debwt = new LexiDeBWT(dane_size);
    debwt->transform(orig_idx, target, source2);
    std::cout << "Source2: " << source2 << std::endl;
  }

  return 0;
}
