#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

struct winsize screen_size;

static void handler_int(int signum) {
  (void) signum;
  printf("Called SIGINT\n");
  exit(EXIT_SUCCESS);
}

static void handler_winch(int signum) {
  (void) signum;
  /* printf("Called SIGWINCH\n"); */
  ioctl(STDIN_FILENO, TIOCGWINSZ, &screen_size);
  printf("Terminal size: %i x %i\n", screen_size.ws_row, screen_size.ws_col);
}

int main(void) {
  // SIGINT handler initialization
  struct sigaction sa_int;
  memset(&sa_int, 0, sizeof(sigaction));
  sa_int.sa_handler = handler_int;
  sigemptyset(&sa_int.sa_mask);
  sa_int.sa_flags = SA_RESTART;
  sigaction(SIGINT, &sa_int, NULL);

  // SIGWINCH handler initialization
  struct sigaction sa_winch;
  memset(&sa_winch, 0, sizeof(sigaction));
  sa_winch.sa_handler = handler_winch;
  sigemptyset(&sa_winch.sa_mask);
  sa_winch.sa_flags = SA_RESTART;
  sigaction(SIGWINCH, &sa_winch, NULL);


  // ensure that file descriptor is terminal
  if (isatty(STDIN_FILENO)) {
    printf("File descriptor IS a tty.\n");
  } else {
    printf("File descriptor IS NOT a tty.\n");
    exit(EXIT_FAILURE);
  }

  while (1) {
    // nothing
  }
  return 0;
}
