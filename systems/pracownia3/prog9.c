#include <stdio.h>
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

#define SOCK_PATH "socket_prog9.socket"
#define LISTEN_BACKLOG 1000
#define BUFFER_SIZE 512

#define N 10

bool numbers[N+1];

int mysock;

void handle_command(char buffer[], int * response_len, char response[]) {
  char command;
  int number;
  sscanf(buffer, "%c %d", &command, &number);
  printf("Command: `%c` `%d`\n", command, number);

  if (command == 'a') {
    int found = false;
    for (int i = 1; i <= N; i++) {
      if (!numbers[i]) {
        numbers[i] = true;
        *response_len = sprintf(response, "%d\n", i);
        found = true;
        break;
      }
      if (!found) {
        *response_len = sprintf(response, "%d\n", 0);
      }
    }
  } else if (command == 'r') {
    if (number > N) {
      *response_len = sprintf(response, "E\n");
    }
    if (numbers[number]) {
      numbers[number] = false;
      *response_len = sprintf(response, "1\n");
    } else {
      *response_len = sprintf(response, "0\n");
    }
  } else {
    *response_len = sprintf(response, "E\n");
  }

  printf("Response: `%s` (%d)\n", response, *response_len);
}

static void handler_int(int signum) {
  (void) signum;
  close(mysock);
  remove(SOCK_PATH);
  exit(0);
}

int main(void) {
  // number data initialization
  for (int i = 0; i <= N; i++) {
    numbers[i] = false;
  }

  // SIGINT handler initialization
  struct sigaction sa;
  memset(&sa, 0, sizeof(sigaction));
  sa.sa_handler = handler_int;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART;
  sigaction(SIGINT, &sa, NULL);

  // socket initialization
  mysock = socket(AF_UNIX, SOCK_DGRAM, 0);
  if (mysock == -1) {
    handle_error("socket");
  }

  // my address initialization
  struct sockaddr_un my_sock_addr;
  memset(&my_sock_addr, 0, sizeof(struct sockaddr_un));
  my_sock_addr.sun_family = AF_UNIX;
  strncpy(my_sock_addr.sun_path, SOCK_PATH, sizeof(my_sock_addr.sun_path) - 1);

  // binding socket to address
  if (bind(mysock, (struct sockaddr *) &my_sock_addr, sizeof(struct sockaddr_un)) == -1) {
    handle_error("bind");
  }

  // client socket initialization
  struct sockaddr_un client_addr;
  memset(&client_addr, 0, sizeof(struct sockaddr_un));
  socklen_t client_addr_len = sizeof(struct sockaddr_un);

  // buffers initialization
  char buffer[BUFFER_SIZE] = {0};
  char response[BUFFER_SIZE] = {0};
  int response_len = 0;

  int len, res;
  while (1) {
    // receiving data
    len = recvfrom(mysock, buffer, BUFFER_SIZE, 0, (struct sockaddr *) &client_addr, &client_addr_len);
    buffer[len] = '\0';

    // handle request
    handle_command(buffer, &response_len, response);

    // send response
    res = sendto(mysock, response, response_len, 0, (struct sockaddr *) &client_addr, client_addr_len);
    if (res == -1) {
      res = errno;
      error(0, res, "foo");
    }
  }

  handler_int(0);

  return 0;
}
