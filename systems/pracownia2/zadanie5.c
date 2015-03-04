#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void exit_handler() {
   printf("Kończę program.");
}

int main(int argc, char *argv[]) {
   atexit(exit_handler);

   if (argc == 2 && strcmp(argv[1], "--exit") == 0) {
      exit(EXIT_SUCCESS);
   } else if (argc == 2 && strcmp(argv[1], "--abort") == 0) {
      abort();
   } else if (argc == 2 && strcmp(argv[1], "--signal") == 0) {
      printf("Czekam na sygnal prez 30 sekund, pid = %d.\n", getpid());
      sleep(30);
   } else if (argc == 2 && strcmp(argv[1], "--fork") == 0) {
      fork();
   } else {
      printf("Dostepne opcje wywolania: --exit, --abort, --signal\n");
   }

   return 0;
}
