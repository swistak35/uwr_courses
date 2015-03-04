#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(void) {
   int makefile_fd = open("Makefile", O_RDONLY);

   char lsof_command[100];
   system(lsof_command);

   pid_t child = fork();

   if (child == 0) {
      // dziecko
      char buf[10];
      size_t nbytes;
      off_t position;
      nbytes = sizeof(buf);

      printf("=== Dziecko: Zaczynam czytac...\n");
      read(makefile_fd, buf, nbytes);
      position = lseek(makefile_fd, 0, SEEK_CUR);
      printf("=== Dziecko: Aktualna pozycja w pliku to %d.\n", (int) position);

      sleep(4);

      printf("=== Dziecko: Zaczynam czytac...\n");
      read(makefile_fd, buf, nbytes);
      position = lseek(makefile_fd, 0, SEEK_CUR);
      printf("=== Dziecko: Aktualna pozycja w pliku to %d.\n", (int) position);

      sleep(30);
   } else {
      // rodzic
      printf("PID: %d\nCHILD: %d\n", getpid(), child);
      sprintf(lsof_command, "lsof -a -p %d,%d Makefile", getpid(), child);
      system(lsof_command);

      sleep(2);

      printf("=== Rodzic: Zaczynam czytac...\n");
      char buf[10];
      size_t nbytes;
      nbytes = sizeof(buf);
      read(makefile_fd, buf, nbytes);
      off_t position = lseek(makefile_fd, 0, SEEK_CUR);
      printf("=== Rodzic: Aktualna pozycja w pliku to %d.\n", (int) position);

      sleep(4);

      printf("=== Rodzic: zamykam...\n");
      close(makefile_fd);
      system(lsof_command);
   }


   sleep(10);
   return 0;
}
