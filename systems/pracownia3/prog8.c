#include <stdio.h>
#include <poll.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/un.h>
#include <stdlib.h>
#include <errno.h>
#include <error.h>

#define handle_error(msg) \
           do { perror(msg); exit(EXIT_FAILURE); } while (0)

#define SOCK_PATH "socket_prog8.socket"
#define LISTEN_BACKLOG 1000
#define BUFFER_SIZE 512

#define N 10
#define TIMEOUT -1

struct pollfd fds[N + 1];
int nfds = 1;

static void handler_int(int signum) {
  close(fds[0].fd);
  remove(SOCK_PATH);
  exit(0);
}

int main(void) {
  // SIGINT handler initialization
  struct sigaction sa;
  memset(&sa, 0, sizeof(sigaction));
  sa.sa_handler = handler_int;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART;
  sigaction(SIGINT, &sa, NULL);

  // main socket initialization
  fds[0].fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fds[0].fd == -1) {
    handle_error("socket");
  }
  fds[0].events = POLLIN;

  // my address initialization
  struct sockaddr_un my_sock_addr;
  memset(&my_sock_addr, 0, sizeof(struct sockaddr_un));
  my_sock_addr.sun_family = AF_UNIX;
  strncpy(my_sock_addr.sun_path, SOCK_PATH, sizeof(my_sock_addr.sun_path) - 1);

  // binding socket to address
  if (bind(fds[0].fd, (struct sockaddr *) &my_sock_addr, sizeof(struct sockaddr_un)) == -1) {
    handle_error("bind");
  }

  char buffer[BUFFER_SIZE] = {0};
  int buffer_len;

  int pres, rt;
  while (1) {
    pres = poll(fds, nfds, TIMEOUT);
    if (pres == 0) {
      // nothing interesting, move along
    } else if (pres == -1) {
      rt = errno;
      if (rt == EINTR) {
        // it's only interrupt, move along
      } else {
        // it's something worse
        error(0, rt, "poll error");
        break;
      }
    } else {
      if (fds[0].revents & POLLIN) {
        // new connection
        fds[nfds].fd = accept(fds[0].fd, NULL, NULL);
        fds[nfds].events = POLLIN;
        nfds++;
      } else {
        for (int i = 1; i < nfds; i++) {
          if (fds[i].revents & POLLIN) {
            // receiving data
            buffer_len = recv(fds[i].fd, buffer, BUFFER_SIZE, 0);
            buffer[buffer_len] = '\0';
            printf("Received: `%s` (%d)\n", buffer, buffer_len);

            // sending data
            sprintf(buffer, "ok (%d)\n", buffer_len);
            pres = send(fds[i].fd, buffer, buffer_len, 0);
            if (pres == -1) {
              rt = errno;
              error(0, rt, "send error");
            }
          } else if (fds[i].revents & POLLHUP) {
            printf("POLLHUP\n");
          }
        }
      }
    }


  }
  /* clisock = accept(mysock, (struct sockaddr_un *) &client_addr, &client_addr_len); */

  /* // buffers initialization */
  /* char response[BUFFER_SIZE] = {0}; */
  /* int response_len = 0; */

  /* int len, res; */
  /* while (1) { */
  /*   // receiving data */
  /*   len = recvfrom(mysock, buffer, BUFFER_SIZE, 0, (struct sockaddr *) &client_addr, &client_addr_len); */
  /*   buffer[len] = '\0'; */

  /*   // handle request */
  /*   handle_command(buffer, &response_len, response); */

  /*   // send response */
  /*   res = sendto(mysock, response, response_len, 0, (struct sockaddr *) &client_addr, client_addr_len); */
  /*   if (res == -1) { */
  /*     res = errno; */
  /*     error(0, res, "foo"); */
  /*   } */
  /* } */

  handler_int(0);

  return 0;
}
