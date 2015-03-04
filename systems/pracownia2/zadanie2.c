#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

void testing_function(void) {
  pid_t pid, zombie;
  pid    = getpid();
  printf("MAIN PID: %d\n", pid);
  zombie = fork();

  pid = getpid();
  if (zombie > 0) {
    // parent
    printf("parent pid: %d\n", pid);
    printf("ps -o state,pid,cmd -p %d,%d\n", pid, zombie);
    getchar();
  } else {
    // child
    printf("child pid: %d\n", pid);
  }
}

int main(int argc, char *argv[]) {
  if (argc == 2 && strcmp(argv[1], "--zombie") == 0) {
    testing_function();
  } else if (argc == 2 && strcmp(argv[1], "--no-zombie") == 0) {
    struct sigaction sa;
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGCHLD, &sa, NULL);
    testing_function();
  } else {
    printf("program has to be run with either --zombie or --no-zombie flag\n");
  }
  return 0;
}
