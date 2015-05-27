#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <time.h>

#define STR_LEN 100
#define ALPHABET_LEN 5
/* #define ITERATIONS 9765625 */
#define ITERATIONS 30517578125

/* void next_perm(char str[STR_LEN]) { */
/*   int i = STR_LEN - 1; */
/*   while (i >= 0) { */
/*     if (str[i] == ALPHABET_LEN - 1) { */
/*       str[i] = 0; */
/*       i--; */
/*     } else { */
/*       str[i]++; */
/*       break; */
/*     } */
/*   } */
/* } */

int current_shuffle = 0;

void next_perm(char str[STR_LEN]) {
  str[current_shuffle] = rand() % ALPHABET_LEN;
  current_shuffle = (current_shuffle + 1) % STR_LEN;

  /* int i = STR_LEN - 1; */
  /* while (i >= 0) { */
  /*   if (str[i] == ALPHABET_LEN - 1) { */
  /*     str[i] = 0; */
  /*     i--; */
  /*   } else { */
  /*     str[i]++; */
  /*     break; */
  /*   } */
  /* } */
}

char mtf_table[ALPHABET_LEN];

void reset_mtf() {
  for (int i = 0; i < ALPHABET_LEN; i++) {
    mtf_table[i] = i;
  }
}

char get_char(char c) {
  int pos;

  for (int i = 0; i < ALPHABET_LEN; i++) {
    if (mtf_table[i] == c) {
      pos = i;
      break;
    }
  }

  for (int i = 0; i < pos; i++) {
    mtf_table[i+1] = mtf_table[i];
  }

  mtf_table[0] = c;
  return pos;
}

void run_mtf(char str[STR_LEN], char mtf[STR_LEN]) {
  for (int i = 0; i < STR_LEN; i++) {
    mtf[i] = get_char(str[i]);
  }
}

int main(void) {
  char str[STR_LEN] = { 0 };
  /* char str[STR_LEN] = { 0, 4, 2, 3, 4, 4, 2, 3, 1, 0 }; */
  char mtf[STR_LEN];
  char str_counts[ALPHABET_LEN] = { 0 };
  char mtf_counts[ALPHABET_LEN] = { 0 };
  srand(time(NULL));

  for (int i = 0; i < ITERATIONS; i++) {
    reset_mtf();
    bzero(str_counts, ALPHABET_LEN);
    bzero(mtf_counts, ALPHABET_LEN);
    run_mtf(str, mtf);

    for (int j = 0; j < STR_LEN; j++) {
      str_counts[str[j]]++;
      mtf_counts[mtf[j]]++;
    }

    if (memcmp(str_counts, mtf_counts, sizeof(char) * ALPHABET_LEN) == 0) {
      printf("Znaleziono: ");
      for (int j = 0; j < STR_LEN; j++) {
        printf("%d ", (int) str[j]);
      }
      printf("\n");
      printf("str_counts: ");
      for (int j = 0; j < ALPHABET_LEN; j++) {
        printf("%d ", (int) str_counts[j]);
      }
      printf("\n");
      printf("mtf_counts: ");
      for (int j = 0; j < ALPHABET_LEN; j++) {
        printf("%d ", (int) mtf_counts[j]);
      }
      printf("\n");
    }

    next_perm(str);
  }

  return 0;
}
