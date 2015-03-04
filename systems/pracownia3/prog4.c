#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/stat.h>
#include <mqueue.h>
#include <time.h>
#include <stdbool.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#define MSG_SIZE 8192

typedef struct cs {
  mqd_t mq;
} cs_t;

cs_t * cs_open(const char *name, int oflag) {
  cs_t * csi;
  csi = (cs_t *) malloc(sizeof(cs_t));
  csi->mq = mq_open(name, oflag, S_IRWXU, NULL);
  return csi;
}

int cs_close(cs_t *csi) {
  int res = mq_close(csi->mq);
  free(csi);
  return res;
}

int cs_wait(cs_t *csi) {
  char msg_ptr[MSG_SIZE];
  unsigned int msg_prio;
  printf("Oczekujemy na wiadomosc...\n");
  int res = mq_receive(csi->mq, msg_ptr, MSG_SIZE, &msg_prio);
  printf("Dostalismy wiadomosc: `%s` (prio: %d)\n", msg_ptr, msg_prio);
  return res;
}

int cs_post(cs_t *csi) {
  int res = mq_send(csi->mq, "ping", 5, 1);
  return res;
}

void * mypthread(void * arg) {
  (void) arg;
  cs_t * csi;
  csi = cs_open("/foobarbaz", O_RDWR | O_CREAT);
  if (csi->mq == (mqd_t) -1) {
    printf("Test: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  sleep(1);
  cs_post(csi);
  cs_close(csi);

  return 0;
}

int main(void) {
  cs_t * csi;
  csi = cs_open("/foobarbaz", O_RDWR | O_CREAT);
  if (csi->mq == (mqd_t) -1) {
    printf("Dupa: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  pthread_t ptid;
  pthread_create(&ptid, NULL, &mypthread, NULL);

  cs_post(csi);
  cs_wait(csi);
  cs_wait(csi);
  cs_close(csi);

  pthread_join(ptid, NULL);

  return 0;
}
