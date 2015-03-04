#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv) {

   int sleep_time, exit_code;
   if (argc == 3) {
      exit_code = atoi(argv[2]);
      sleep_time = atoi(argv[1]);
   } else if (argc == 2) {
      exit_code = 0;
      sleep_time = atoi(argv[1]);
   } else {
      exit_code = 0;
      sleep_time = 10;
   }

   printf("Sleeper start pid = %d | exit_code = %d | sleep = %d\n", getpid(), exit_code, sleep_time);
   sleep(sleep_time);
   sleep(sleep_time);
   printf("Sleeper end\n");

   return exit_code;
}
