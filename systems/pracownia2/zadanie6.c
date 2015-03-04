#include <unistd.h>
#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>

extern char ** environ;

int main(int argc, char** argv) {
   printf("pid: %d\n", getpid());

   (void) argc;
   int status, real_status;
   pid_t child;
   child = fork();

   if (child == 0) {
      status = execve(argv[1], argv + 1, environ);

      if(status == -1) {
         perror("execve error");
      }
   } else {
      wait(&status);

      printf("Status = %d\n", status);

      if (WIFEXITED(status)) {
         real_status = WEXITSTATUS(status);
      } else if (WIFSIGNALED(status)) {
         printf("Signal %d\n", WTERMSIG(status));
         real_status = 128 + WTERMSIG(status);
      } else {
         real_status = status;
      }

      printf("Real status = %d\n", real_status);
   }

   return 0;
}
