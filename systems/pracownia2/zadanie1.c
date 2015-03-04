#include <unistd.h>
#include <stdio.h>

int main(void) {
  pid_t pid, ppid, sid, sid2, pgrp;

  pid = getpid();
  printf("PID: %d\n", pid);

  ppid = getppid();
  printf("PPID: %d\n", ppid);

  sid = getsid((pid_t)0);
  sid2 = getsid(pid);
  printf("SID: %d\n", sid);
  printf("SID: %d (alternative)\n", sid2);

  pgrp = getpgrp();
  printf("PGRP: %d\n", pgrp);

  printf("Check out this!\n");
  printf("ps -o pid,ppid,sid,pgrp,cmd -p %d\n", pid);

  getchar();
  return 0;
}
