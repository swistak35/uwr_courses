#include <sys/stat.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int sockets[2];

#define print(X) (write(STDOUT_FILENO, X, sizeof(X)))
#define println(X) (print(X), print("\n"))

int itoa(int i, char tab[]) {
  char const digit[] = "0123456789";
  int shifter = i;
  char * p = tab;
  int len = 0;

  do {
    ++p;
    shifter = shifter/10;
    len++;
  } while (shifter);

  *p = '\0';
  do {
    *--p = digit[i%10];
    i = i/10;
  } while(i);

  return len;
}

void print_piece(int fd) {
  int pid = getpid();
  char buffer[8];

  itoa(pid, buffer);
  print(buffer);
  print(" ");
  read(fd, buffer, 8);
  println(buffer);
}

void push_msg(int socket, int fd) {
  struct msghdr msg = { 0 };
  struct cmsghdr * cmsg;

  union {
    char   buf[CMSG_SPACE(sizeof(int))];
    struct cmsghdr align;
  } u;

  int *fdptr;

  msg.msg_control = u.buf;
  msg.msg_controllen = sizeof(u.buf);

  cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(sizeof(int));

  fdptr = (int *) CMSG_DATA(cmsg);
  *fdptr = fd;
  msg.msg_controllen = cmsg->cmsg_len;

  sendmsg(socket, &msg, 0);
}

void pull_msg(int socket, int * fd) {
  struct msghdr msg = { 0 };
  struct cmsghdr * cmsg;

  union {
    char   buf[CMSG_SPACE(sizeof(int))];
    struct cmsghdr align;
  } u;

  msg.msg_control = u.buf;
  msg.msg_controllen = sizeof(u.buf);

  recvmsg(socket, &msg, 0);

  cmsg = CMSG_FIRSTHDR(&msg);
  if (cmsg == NULL) {
    println("NULL in cmsg");
  }

  *fd = *((int *) CMSG_DATA(cmsg));
}

void child1() {
  int fd = open("pliczek", O_RDONLY);
  print_piece(fd);
  push_msg(sockets[0], fd);
}

void child2() {
  int fd;
  pull_msg(sockets[1], &fd);
  print_piece(fd);
}

int main(void) {
  int err;
  err = socketpair(AF_UNIX, SOCK_DGRAM, 0, sockets);

  if (err) {
    return 1;
  }

  if (fork() == 0) {
    // child 1
    child1();
  } else {

    // parent
    if (fork() == 0) {

      // child 2
      child2();
    }
  }

  int status1, status2;
  wait(&status1);
  wait(&status2);

  return (status1 + status2);
}
