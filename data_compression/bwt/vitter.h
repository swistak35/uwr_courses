#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>

#define __cdecl

typedef struct {
    unsigned int up,      // next node up the tree
        down,         // pair of down nodes
        symbol,       // node symbol value
        weight;       // node weight
} HTable;

typedef struct {
    unsigned int esc,     // the current tree height
        root,         // the root of the tree
        size,         // the alphabet size
        * map;         // mapping for symbols to nodes
    HTable table[1];  // the coding table starts here
} HCoder;

class Huffman {
  public:
    Huffman(FILE * huffman_source, FILE * huffman_target);
    ~Huffman();
    void huff_init(unsigned int size, unsigned int root);
    unsigned int huff_split(HCoder * huff, unsigned int symbol);
    void huff_increment(HCoder * huff, unsigned int node);
    void huff_scale(unsigned int bits);
    void huff_sendid (HCoder *huff, unsigned symbol);
    void huff_encode(unsigned int symbol);
    unsigned huff_readid (HCoder *huff);
    unsigned huff_decode();
    void arc_put1 (unsigned bit);
    unsigned int arc_get1();
    void compress();
    void decompress();
    HCoder * hcoder;
    unsigned char ArcBit;
    int ArcChar;
    FILE * Out;
    FILE * In;
};
