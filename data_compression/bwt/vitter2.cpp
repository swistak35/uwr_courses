#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>

#define HUFFSTANDALONE 1
#define __cdecl

void arc_put1(unsigned bit);
unsigned arc_get1();

//  This code is adapted from Professor Vitter's
//  article, Design and Analysis of Dynamic Huffman Codes,
//  which appeared in JACM October 1987

//  A design trade-off has been made to simplify the
//  code:  a node's block is determined dynamically,
//  and the implicit tree structure is maintained,
//  e.g. explicit node numbers are also implicit.

//  Dynamic huffman table weight ranking
//  is maintained per Professor Vitter's
//  invariant (*) for algorithm FGK:

//  leaves preceed internal nodes of the
//  same weight in a non-decreasing ranking
//  of weights using implicit node numbers:

//  1) leaves slide over internal nodes, internal nodes
//  swap over groups of leaves, leaves are swapped
//  into group leader position, but two internal
//  nodes never change positions relative
//  to one another.

//  2) weights are incremented by 2:
//  leaves always have even weight values;
//  internal nodes always have odd values.

//  3) even node numbers are always right children;
//  odd numbers are left children in the tree.

//  node 2 * HuffSize - 1 is always the tree root;
//  node HuffEsc is the escape node;

//  the tree is initialized by creating an
//  escape node as the root.

//  each new leaf symbol is paired with a new escape
//  node into the previous escape node in the tree,
//  until the last symbol which takes over the
//  tree position of the escape node, and
//  HuffEsc is left at zero.

//  overall table size: 2 * HuffSize

//  huff_init(alphabet_size, potential symbols used)
//  huff_encode(next_symbol)
//  next_symbol = huff_decode()

//  huff_scale(by_bits) -- scale weights and rebalance tree

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

//  initialize an adaptive coder
//  for alphabet size, and count
//  of nodes to be used

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

Huffman::Huffman(FILE * huffman_source, FILE * huffman_target) {
  this->In = huffman_source;
  this->Out = huffman_target;
  ArcBit = 0;
  ArcChar = 0;
}

Huffman::~Huffman() {
}

void Huffman::huff_init(unsigned int size, unsigned int root) {
  HCoder * huff;

  //  default: all alphabet symbols are used
  if( !root || root > size ) {
    root = size;
  }

  //  create the initial escape node
  //  at the tree root

  if( root <<= 1 ) {
    root--;
  }

  huff = (HCoder *) malloc(root * sizeof(HTable) + sizeof(HCoder));
  memset(huff->table + 1, 0, root * sizeof(HTable));
  memset(huff, 0, sizeof(HCoder));

  huff->size = size;
  if( huff->size ) {
    huff->map = (unsigned int *) calloc(size, sizeof(unsigned int));
  }

  huff->esc = huff->root = root;
  this->hcoder = huff;
}

// split escape node to incorporate new symbol

unsigned int Huffman::huff_split(HCoder *huff, unsigned symbol) {
  unsigned pair, node;

  //  is the tree already full???

  pair = huff->esc;
  if( pair ) {
    huff->esc--;
  } else {
    return 0;
  }

  //  if this is the last symbol, it moves into
  //  the escape node's old position, and
  //  huff->esc is set to zero.

  //  otherwise, the escape node is promoted to
  //  parent a new escape node and the new symbol.

  node = huff->esc;
  if( node ) {
    huff->table[pair].down = node;
    huff->table[pair].weight = 1;
    huff->table[node].up = pair;
    huff->esc--;
  } else {
    pair = 0;
    node = 1;
  }

  //  initialize the new symbol node

  huff->table[node].symbol = symbol;
  huff->table[node].weight = 0;
  huff->table[node].down = 0;
  huff->map[symbol] = node;

  //  initialize a new escape node.

  huff->table[huff->esc].weight = 0;
  huff->table[huff->esc].down = 0;
  huff->table[huff->esc].up = pair;
  return node;
}

//  swap leaf to group leader position
//  return symbol's new node

unsigned int huff_leader(HCoder * huff, unsigned int node) {
  unsigned int weight = huff->table[node].weight;
  unsigned int leader = node, prev, symbol;

  while ( weight == huff->table[leader + 1].weight ) {
    leader++;
  }

  if ( leader == node ) {
    return node;
  }

  // swap the leaf nodes

  symbol = huff->table[node].symbol;
  prev = huff->table[leader].symbol;

  huff->table[leader].symbol = symbol;
  huff->table[node].symbol = prev;
  huff->map[symbol] = leader;
  huff->map[prev] = node;
  return leader;
}

//  slide internal node up over all leaves of equal weight;
//  or exchange leaf with next smaller weight internal node

//  return node's new position

unsigned int huff_slide (HCoder * huff, unsigned int node) {
  unsigned next = node;
  HTable swap[1];

  *swap = huff->table[next++];

  // if we're sliding an internal node, find the
  // highest possible leaf to exchange with

  if ( swap->weight & 1 ) {
    while( swap->weight > huff->table[next + 1].weight ) {
      next++;
    }
  }

  //  swap the two nodes

  huff->table[node] = huff->table[next];
  huff->table[next] = *swap;

  huff->table[next].up = huff->table[node].up;
  huff->table[node].up = swap->up;

  //  repair the symbol map and tree structure

  if( swap->weight & 1 ) {
    huff->table[swap->down].up = next;
    huff->table[swap->down - 1].up = next;
    huff->map[huff->table[node].symbol] = node;
  } else {
    huff->table[huff->table[node].down - 1].up = node;
    huff->table[huff->table[node].down].up = node;
    huff->map[swap->symbol] = next;
  }

  return next;
}

//  increment symbol weight and re balance the tree.

void Huffman::huff_increment(HCoder * huff, unsigned int node) {
  unsigned up;

  //  obviate swapping a parent with its child:
  //    increment the leaf and proceed
  //    directly to its parent.

  //  otherwise, promote leaf to group leader position in the tree

  if( huff->table[node].up == node + 1 ) {
    huff->table[node].weight += 2, node++;
  } else {
    node = huff_leader(huff, node);
  }

  //  increase the weight of each node and slide
  //  over any smaller weights ahead of it
  //  until reaching the root

  //  internal nodes work upwards from
  //  their initial positions; while
  //  symbol nodes slide over first,
  //  then work up from their final
  //  positions.

  while( huff->table[node].weight += 2, up = huff->table[node].up ) {
    while( huff->table[node].weight > huff->table[node + 1].weight ) {
      node = huff_slide (huff, node);
    }

    if( huff->table[node].weight & 1 ) {
      node = up;
    } else {
      node = huff->table[node].up;
    }
  }
}

//  scale all weights and rebalance the tree

//  zero weight nodes are removed from the tree
//  by sliding them out the left of the rank list

void Huffman::huff_scale(unsigned int bits) {
  HCoder * huff = this->hcoder;
  unsigned int node = huff->esc, weight, prev;

  //  work up the tree from the escape node
  //  scaling weights by the value of bits

  while( ++node <= huff->root ) {
    //  recompute the weight of internal nodes;
    //  slide down and out any unused ones

    if( huff->table[node].weight & 1 ) {
      weight = huff->table[huff->table[node].down].weight & ~1;
      if( weight ) {
        weight += huff->table[huff->table[node].down - 1].weight | 1;
      }

    //  remove zero weight leaves by incrementing HuffEsc
    //  and removing them from the symbol map.  take care

    } else if( !(weight = huff->table[node].weight >> bits & ~1) )
      if( huff->map[huff->table[node].symbol] = 0, huff->esc++ )
        huff->esc++;

    // slide the scaled node back down over any
    // previous nodes with larger weights

    huff->table[node].weight = weight;
    prev = node;

    while( weight < huff->table[--prev].weight )
        huff_slide (huff, prev);
  }

  // prepare a new escape node

  huff->table[huff->esc].down = 0;
}

//  send the bits for an escaped symbol

void Huffman::huff_sendid (HCoder *huff, unsigned symbol) {
  unsigned empty = 0, max;

    //  count the number of empty symbols
    //  before the symbol in the table

    while( symbol-- )
      if( !huff->map[symbol] )
        empty++;

    //  send LSB of this count first, using
    //  as many bits as are required for
    //  the maximum possible count

    max = huff->size - (huff->root - huff->esc) / 2 - 1;
    if( max ) {
      do arc_put1 (empty & 1), empty >>= 1;
      while( max >>= 1 );
    }
}

//  encode the next symbol

void Huffman::huff_encode(unsigned int symbol) {
  HCoder * huff = this->hcoder;
  unsigned emit = 1, bit;
  unsigned up, idx, node;

    if( symbol < huff->size )
        node = huff->map[symbol];
    else
        return;

    //  for a new symbol, direct the receiver to the escape node
    //  but refuse input if table is already full.

    if( !(idx = node) )
      if( !(idx = huff->esc) )
        return;

    //  accumulate the code bits by
    //  working up the tree from
    //  the node to the root

    up = huff->table[idx].up;
    while( up ) {
      emit <<= 1, emit |= idx & 1, idx = up;
      up = huff->table[idx].up;
    }

    //  send the code, root selector bit first

    while( bit = emit & 1, emit >>= 1 )
        arc_put1 (bit);

    //  send identification and incorporate
    //  new symbols into the tree

    if( !node )
        huff_sendid(huff, symbol), node = huff_split(huff, symbol);

    //  adjust and re-balance the tree

    huff_increment (huff, node);
}

//  read the identification bits
//  for an escaped symbol

unsigned Huffman::huff_readid (HCoder *huff)
{
unsigned empty = 0, bit = 1, max, symbol;

    //  receive the symbol, LSB first, reading
    //  only the number of bits necessary to
    //  transmit the maximum possible symbol value

    max = huff->size - (huff->root - huff->esc) / 2 - 1;
    if( max ) {
      do empty |= arc_get1 () ? bit : 0, bit <<= 1;
      while( max >>= 1 );
    }

    //  the count is of unmapped symbols
    //  in the table before the new one

    for( symbol = 0; symbol < huff->size; symbol++ )
      if( !huff->map[symbol] )
        if( !empty-- )
            return symbol;

    //  oops!  our count is too big, either due
    //  to a bit error, or a short node count
    //  given to huff_init.

    return 0;
}

//  decode the next symbol

unsigned Huffman::huff_decode ()
{
  HCoder * huff = this->hcoder;
  unsigned node = huff->root;
  unsigned symbol, down;

    //  work down the tree from the root
    //  until reaching either a leaf
    //  or the escape node.  A one
    //  bit means go left, a zero
    //  means go right.

    down = huff->table[node].down;
    while( down ) {
      if( arc_get1 () ) {
        node = down - 1;  // the left child preceeds the right child
      } else {
        node = down;
      }
      down = huff->table[node].down;
    }

    //  sent to the escape node???
    //  refuse to add to a full tree

    if( node == huff->esc )
      if( huff->esc )
        symbol = huff_readid (huff), node = huff_split (huff, symbol);
      else
        return 0;
    else
        symbol = huff->table[node].symbol;

    //  increment weights and rebalance
    //  the coding tree

    huff_increment (huff, node);
    return symbol;
}


/* int main (int argc, char **argv) { */
/*   int mode; */

/*     if( argc > 1 ) { */
/*       mode = argv[1][0], argv[1]++; */
/*     } */
    
/*     FILE * In = fopen(argv[2], "r"); */
/*     FILE * Out = fopen(argv[3], "w"); */

/*     Huffman * huffman = new Huffman(In, Out); */

/*     if( mode == 'd' ) { */
/*       huffman->decompress(); */
/*     } else if (mode == 'c') { */
/*       huffman->compress(); */
/*     } else { */
/*       printf("BLAD!!!\n"); */
/*     } */

/*     return 0; */
/* } */

void Huffman::arc_put1 (unsigned bit) {
    ArcChar <<= 1;

    if(bit) {
      ArcChar |= 1;
    }

    ArcBit++;
    if(ArcBit < 8) {
      return;
    }

    putc(ArcChar, Out);
    ArcChar = 0;
    ArcBit = 0;
}

void Huffman::compress() {
  unsigned int size = 256;
unsigned mask = ~0;
int symbol;
  huff_init (256, size);
  putc (size >> 8, Out);
  putc (size, Out);

  fseek(In, 0, 2);
  size = ftell(In);
  fseek (In, 0, 0);

  putc (size >> 16, Out);
  putc (size >> 8, Out);
  putc (size, Out);

  while( size )
    if( symbol = getc(In), huff_encode(symbol), size-- & mask )
      continue;
    else
      huff_scale(1);

  while( ArcBit )  // flush last few bits
      arc_put1 (0);
}

void Huffman::decompress() {
  int size = 256;
unsigned mask = ~0;
int symbol;
  size = getc(In) << 8;
  size |= getc(In);

  huff_init (256, size);

  size = getc(In) << 16;
  size |= getc(In) << 8;
  size |= getc(In);

  while( size )
    if( symbol = huff_decode(), putc (symbol, Out), size-- & mask )
      continue;
    else
      huff_scale(1);
}

unsigned int Huffman::arc_get1() {
    if(!ArcBit) {
      ArcChar = getc(In);
      ArcBit = 8;
    }

    return ArcChar >> --ArcBit & 1;
}
