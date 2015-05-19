#ifndef VITTER_H
#define VITTER_H
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>

/* #define HUFFSTANDALONE 1 */
#define __cdecl

typedef struct {
    unsigned up,      // next node up the tree
        down,         // pair of down nodes
        symbol,       // node symbol value
        weight;       // node weight
} HTable;

typedef struct {
    unsigned esc,     // the current tree height
        root,         // the root of the tree
        size,         // the alphabet size
        *map;         // mapping for symbols to nodes
    HTable table[1];  // the coding table starts here
} HCoder;

HCoder *huff_init (unsigned size, unsigned root);
void huff_encode (HCoder *huff, unsigned symbol);
unsigned huff_decode (HCoder *huff);
void huff_scale (HCoder *huff, unsigned bits);
#endif
