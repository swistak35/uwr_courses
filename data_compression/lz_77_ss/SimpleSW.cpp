#include "SimpleSW.h"

using namespace std;

SimpleSW::SimpleSW(WindowResult * result) {
  this->result = result;
}

SimpleSW::~SimpleSW() {
}

void SimpleSW::set_dict_buf(unsigned char * dict_buf, int * dict_buf_idx,
    int * dict_buf_size, int dict_buf_max_size) {
  this->dict_buf = dict_buf;
  this->dict_buf_idx = dict_buf_idx;
  this->dict_buf_size = dict_buf_size;
  this->dict_buf_max_size = dict_buf_max_size;
}

void SimpleSW::set_input_buf(unsigned char * input_buf, int * input_buf_idx,
    int * input_buf_size, int input_buf_max_size) {
  this->input_buf = input_buf;
  this->input_buf_idx = input_buf_idx;
  this->input_buf_size = input_buf_size;
  this->input_buf_max_size = input_buf_max_size;
}

void SimpleSW::step() {
  int longestLength = 0;
  int longestIndex = -1;

  // searching for lcs
  {
    int dict_idx = *dict_buf_idx;

    while (dict_idx < *dict_buf_size) {
      int length = find_lcs(dict_idx);
      if (length >= longestLength && length > 0) {
        longestLength = length;
        longestIndex = dict_idx;
      }
      dict_idx++;
    }

    dict_idx = 0;
    while (dict_idx < *dict_buf_idx) {
      int length = find_lcs(dict_idx);
      if (length >= longestLength && length > 0) {
        longestLength = length;
        longestIndex = dict_idx;
      }
      dict_idx++;
    }
  }

  printf("++ longest found: %d %d\n", longestIndex, longestLength);

  // saving the result
  {
    this->result->length = longestLength;
    if (longestIndex == -1) {
      this->result->offset = 0;
    } else if (longestIndex < *dict_buf_idx) {
      this->result->offset = *dict_buf_size - (*dict_buf_size - *dict_buf_idx) - longestIndex;
    } else {
      this->result->offset = *dict_buf_idx + (*dict_buf_size - longestIndex);
    }
  }

  // copying the input data to dict
  {
    int chars_to_copy = longestLength;
    while (chars_to_copy > 0 && *dict_buf_size < dict_buf_max_size) {
      insert_char_into_dict(input_buf[*input_buf_idx]);
      *input_buf_idx = wrap(*input_buf_idx + 1, *input_buf_size);
      chars_to_copy--;
    }
    
    while (chars_to_copy > 0) {
      insert_char_into_dict(input_buf[*input_buf_idx]);
      *input_buf_idx = wrap(*input_buf_idx + 1, *input_buf_size);
      chars_to_copy--;
    }
  }
}

void SimpleSW::insert_char_into_dict(unsigned char c) {
  if (*dict_buf_size < dict_buf_max_size) {
    dict_buf[*dict_buf_size] = c;
    (*dict_buf_size)++;
  } else {
    dict_buf[*dict_buf_idx] = c;
    *dict_buf_idx = wrap(*dict_buf_idx + 1, *dict_buf_size);
  }
}

int SimpleSW::find_lcs(int starting_dict_idx) {
  int dict_idx = starting_dict_idx;
  int input_idx = *input_buf_idx;
  int length = 0;
  while (dict_buf[dict_idx] == input_buf[input_idx]) {
    dict_idx++;
    input_idx++;
    length++;
    if (dict_idx >= *dict_buf_size) {
      dict_idx = 0;
    }
    if (input_idx >= *input_buf_size) {
      input_idx = 0;
    }
    if (input_idx == *input_buf_idx) {
      break;
    }
    if (dict_idx == *dict_buf_idx) {
      break;
    }
  }
  return length;
}

int SimpleSW::wrap(int x, int size) {
  if (x >= size) {
    return (x - size);
  } else if (x < 0) {
    return (x + size);
  } else {
    return x;
  }
}

int SimpleSW::upper_bound(int x, int bound) {
  if (x > bound) {
    return bound;
  } else {
    return x;
  }
}
