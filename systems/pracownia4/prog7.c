#include <stdlib.h>
#include <sys/inotify.h>
#include <stdio.h>
#include <errno.h>

int intf;

int main(void) {
  int res;

  intf = inotify_init1(0);
  res = inotify_add_watch(intf, "testkatalog", IN_ALL_EVENTS);
  if (res < 0) {
    perror("add watch");
  }


  close(intf);

  return 0;
}
