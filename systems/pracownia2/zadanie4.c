#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/resource.h>

#define FORK_LIMIT 50

int main(void) {
  pid_t ppid, child;
  ppid = getpid();
  setpgid(ppid, 0);
  printf("pid: %d\n", ppid);

  struct rlimit proc_lim;

  proc_lim.rlim_cur = (rlim_t) FORK_LIMIT;
  proc_lim.rlim_max = (rlim_t) FORK_LIMIT;

  setrlimit(RLIMIT_NPROC, &proc_lim);

  for (int i = 0; i < 50; i++) {
    child = fork();
    if (child == 0) {
      // child
      printf("child pid: %d pgid: %d\n", getpid(), getpgrp());
      pause();
      return 0;
    } else if (child == -1) {
      perror("child not created");
    }
  }

  signal(SIGTERM, SIG_IGN);
  sleep(30);
  killpg(ppid, SIGTERM);
  printf("I killed my own children.\n");
  sleep(60);
  return 0;
}
