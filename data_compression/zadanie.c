#include <stdio.h>
#include <string.h>

#define STR_LEN 15
#define ALPHABET_LEN 5
/* #define ITERATIONS 9765625 */
#define ITERATIONS 30517578125

void next_perm(char str[STR_LEN]) {
  int i = STR_LEN - 1;
  while (i >= 0) {
    if (str[i] == ALPHABET_LEN - 1) {
      str[i] = 0;
      i--;
    } else {
      str[i]++;
      break;
    }
  }
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

  for (int i = 0; i < ITERATIONS; i++) {
    reset_mtf();
    run_mtf(str, mtf);

    if (memcmp(str, mtf, sizeof(char) * STR_LEN) == 0) {
      printf("Znaleziono: ");
      for (int j = 0; j < STR_LEN; j++) {
        printf("%d ", (int) mtf[j]);
      }
      printf("\n");
    }

    next_perm(str);
  }

  return 0;
}
