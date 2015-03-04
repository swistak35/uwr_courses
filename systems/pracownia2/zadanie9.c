#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

void handler_term(int signum) {
   printf("Zlapalem sygnal %d, powinien to byc SIGTERM\n", signum);
}

int main(void) {
   pid_t zombie = fork();

   if (zombie == 0) {
      // podproces
      struct sigaction sa_int;
      sa_int.sa_handler = SIG_IGN;
      sa_int.sa_flags   = 0;
      sigemptyset(&sa_int.sa_mask);
      sigaction(SIGINT, &sa_int, NULL);

      struct sigaction sa_term;
      sa_term.sa_handler = handler_term;
      sa_term.sa_flags   = SA_RESETHAND;
      sigemptyset(&sa_term.sa_mask);
      sigaction(SIGTERM, &sa_term, NULL);

      sleep(10);
      sleep(10);
   } else {
      // rodzic

      char ps_command[100];
      sprintf(ps_command, "ps --no-headers -o pid,cmd -p %d,%d", getpid(), zombie);

      sleep(2);
      printf("Wysylam SIGINT\n");
      kill(zombie, SIGINT);
      system(ps_command);

      sleep(2);
      printf("Wysylam SIGTERM\n");
      kill(zombie, SIGTERM);
      system(ps_command);

      /* sleep(2); */
      /* printf("Wysylam SIGTERM\n"); */
      /* kill(zombie, SIGTERM); */
      /* system(ps_command); */

      sleep(2);
      printf("Wysylam SIGKILL\n");
      kill(zombie, SIGKILL);
      system(ps_command);

      sleep(2);
   }
   return 0;
}
