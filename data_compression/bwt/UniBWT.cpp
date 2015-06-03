#include "UniBWT.h"
#include <iostream>

using namespace std;

UniBWT::UniBWT(int length) {
  this->length = length;
  this->ranks = (int *) calloc(this->length, sizeof(int));
  this->new_ranks = (int *) calloc(this->length, sizeof(int));
  this->positions = (int *) calloc(this->length, sizeof(int));
  if (this->length < 256) {
    this->hvecs = 256;
  } else {
    this->hvecs = this->length;
  }
  this->hvec.assign(this->hvecs, std::vector<int>(0));
  if (this->ranks == NULL || this->positions == NULL || this->new_ranks == NULL) {
    printf("nieudane zajecie pamieci\n");
  }
}

UniBWT::~UniBWT() {
}

int UniBWT::transform(unsigned char * source, int * target) {
  this->source = source;
  this->target = target;

  sort();

  return this->ranks[0];
}

void UniBWT::sort() {
  // Init vectors
  /* bzero(this->ranks, sizeof(int) * this->length); */

  for (int i = 0; i < this->length; i++) {
    this->positions[i] = i;
  }

  // first iteration
  {
    for (int i = 0; i < 256; i++) { // wlasciwie to wystarczy tylko 256 pierwszych, chyba ze length < 256
      hvec[i].clear();
    }

    for (int i = 0; i < this->length; i++) {
      unsigned char c = this->source[get_char_idx(i - 1)];
      this->hvec[c].push_back(i);
    }

    int j = 0;
    for (int i = 0; i < 256; i++) {
      for (int v : this->hvec[i]) {
        this->positions[j] = v;
        this->ranks[v] = i;
        j++;
      }
    }
  }
  int buckets = 256;

  int k = 0;
  int offset = 1 << k;
  while (offset <= this->length / 2) {
    printf("k = %d | offset = %d\n", k, offset);

    // moze wystarczy utrzymywac wartosc previous_buckets i tylko te czyscic
    for (int i = 0; i < buckets; i++) {
      hvec[i].clear();
    }

    for (int i = 0; i < this->length; i++) {
      int rank = this->ranks[get_char_idx(this->positions[i] - offset)];
      this->hvec[rank].push_back(this->positions[i]);
    }

    int j = 0;
    int current_rank = -1;
    int prev_rank_left = -1;
    int prev_rank_right = -1;
    for (int i = 0; i < buckets; i++) {
      for (int v : this->hvec[i]) {
        this->positions[j] = v;
        int rank_right = this->ranks[v];
        int rank_left = this->ranks[get_char_idx(v - offset)];
        if (rank_left != prev_rank_left || rank_right != prev_rank_right) {
          current_rank++;
          prev_rank_left = rank_left;
          prev_rank_right = rank_right;
        }
        this->new_ranks[v] = current_rank;
        j++;
      }
    }
    buckets = current_rank + 1;

    for (int i = 0; i < this->length; i++) {
      this->ranks[i] = this->new_ranks[i];
    }

    k++;
    offset <<= 1;
  }

  int it = this->length - offset - 1;
  while (it >= 0) {
    for (int i = 0; i < 256; i++) {
      hvec[i].clear();
    }

    for (int i = 0; i < this->length; i++) {
      unsigned char c = this->source[get_char_idx(this->positions[i] + it)];
      this->hvec[c].push_back(this->positions[i]);
    }

    {
      int j = 0;
      for (int i = 0; i < 256; i++) {
        for (int v : this->hvec[i]) {
          this->positions[j] = v;
          j++;
        }
      }
    }

    it--;
  }

  for (int i = 0; i < this->length; i++) {
    this->ranks[this->positions[i]] = i;
  }

  this->target[this->ranks[0]] = this->source[this->length-1];
  for (int i = 1; i < this->length; i++) {
    this->target[this->ranks[i]] = this->source[i-1];
  }
}

int UniBWT::get_char_idx(int idx) {
  if (idx >= this->length) {
    return (idx - this->length);
  } else if (idx < 0) {
    return (idx + this->length);
  } else {
    return idx;
  }
}
