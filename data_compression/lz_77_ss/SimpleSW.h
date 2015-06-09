#ifndef SIMPLE_SW_H
#define SIMPLE_SW_H

/* #include <algorithm> */
/* #include <list> */
/* #include <vector> */
/* #include <iostream> */
/* #include <cmath> */
#include <cstdio>

using namespace std;

typedef struct {
  int offset;
  int length;
} WindowResult;

typedef struct {
  unsigned char * buf;
  int begin;
  int size;
  int max;
} CircularBuffer;

class SimpleSW {
  public:
    SimpleSW(WindowResult * result);
    ~SimpleSW();
    void step();
    void insert_char_into_dict(unsigned char c);
    void set_input_buf(unsigned char * input_buf, int * input_buf_idx, int * input_buf_size, int input_buf_max_size);
    void set_dict_buf(unsigned char * dict_buf, int * dict_buf_idx, int * dict_buf_size, int dict_buf_max_size);
  private:
    int find_lcs(int starting_dict_idx);
    int wrap(int x, int size);
    int upper_bound(int x, int bound);
    // pointers to the buffers
    unsigned char * dict_buf;
    unsigned char * input_buf;
    // pointer to the beginning of the buffer, i.e. first character.
    int * dict_buf_idx;
    int * input_buf_idx;
    // size of the buffer
    int * dict_buf_size;
    int * input_buf_size;
    // max size of the buffer
    int dict_buf_max_size;
    int input_buf_max_size;
    WindowResult * result;
};

#endif

// refactoring:
// - uzyc CircularBuffer
