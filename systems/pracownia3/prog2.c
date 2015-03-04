#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <time.h>
#include <stdbool.h>
#include <unistd.h>

#define THREADS_AMOUNT 3
#define MAX_THREADS 1000
#define RUNS_AMOUNT 10
#define THREADS_EACH_RUN 20
#define READER_PROBABILITY 50
#define RUN_WAIT_MIN 500
#define RUN_WAIT_MAX 1500

#define NS_IN_US 1000

int reader_max = 0;
int writer_max = 0;

bool is_reader;
int res;

typedef struct fifo_mutex {
  pthread_cond_t cond;
  pthread_mutex_t mutex;
  int qhead, qtail;
} fifo_mutex_t;

void fifo_lock(fifo_mutex_t *fifo) {
  pthread_mutex_lock(&fifo->mutex);
  int qpos = fifo->qtail++;
  while (qpos != fifo->qhead) {
    pthread_cond_wait(&fifo->cond, &fifo->mutex);
  }
  pthread_mutex_unlock(&fifo->mutex);
}

void fifo_unlock(fifo_mutex_t *fifo) {
  pthread_mutex_lock(&fifo->mutex);
  fifo->qhead++;
  pthread_cond_broadcast(&fifo->cond);
  pthread_mutex_unlock(&fifo->mutex);
}

pthread_mutex_t mx_reader_max, mx_writer_max;
fifo_mutex_t writing_queue, reading_queue;

int writer_count = 0;
int reader_count = 0;
pthread_mutex_t mx_writer_count, mx_reader_count;

void random_wait(int min, int max) {
  struct timespec sleep_time;
  sleep_time.tv_sec = 0;
  sleep_time.tv_nsec = ((rand() % (max - min)) + min) * NS_IN_US;
  nanosleep(&sleep_time, NULL);
}

void * reader_start(void * arg) {
  (void) arg;
  clock_t t1, t2;
  long diff;
  t1 = clock();

  fifo_lock(&reading_queue);
  pthread_mutex_lock(&mx_reader_count);
  reader_count++;
  if (reader_count == 1) {
    fifo_lock(&writing_queue);
  }
  pthread_mutex_unlock(&mx_reader_count);
  fifo_unlock(&reading_queue);

  t2 = clock();
  diff = (t2 - t1) * 1000000 / CLOCKS_PER_SEC;

  pthread_mutex_lock(&mx_reader_max);
  if (diff > reader_max) {
    reader_max = diff;
  }
  pthread_mutex_unlock(&mx_reader_max);

  random_wait(10, 100);


  pthread_mutex_lock(&mx_reader_count);
  reader_count--;
  if (reader_count == 0) {
    fifo_unlock(&writing_queue);
  }
  pthread_mutex_unlock(&mx_reader_count);

  return 0;
}

void * writer_start(void * arg) {
  (void) arg;
  clock_t t1, t2;
  long diff;
  t1 = clock();

  pthread_mutex_lock(&mx_writer_count);
  writer_count++;
  pthread_mutex_unlock(&mx_writer_count);

  fifo_lock(&reading_queue);
  fifo_lock(&writing_queue);

  /* fifo_lock(&mx_resource); */

  t2 = clock();
  diff = (t2 - t1) * 1000000 / CLOCKS_PER_SEC;

  pthread_mutex_lock(&mx_writer_max);
  if (diff > writer_max) {
    writer_max = diff;
  }
  pthread_mutex_unlock(&mx_writer_max);

  random_wait(10, 100);

  fifo_unlock(&writing_queue);
  fifo_unlock(&reading_queue);

  /* fifo_unlock(&mx_resource); */

  pthread_mutex_lock(&mx_writer_count);
  writer_count--;
  pthread_mutex_unlock(&mx_writer_count);

  return 0;
}

pthread_t pt_ids[RUNS_AMOUNT * THREADS_EACH_RUN];
int total = 0;

int main() {
  srand(time(NULL));


  for (int i = 0; i < RUNS_AMOUNT; i++) {
    for (int j = 0; j < THREADS_EACH_RUN; j++) {
      if (writer_count + reader_count < MAX_THREADS) {
        is_reader = ((rand() % 100) < READER_PROBABILITY);

        if (is_reader) {
          pthread_create(&pt_ids[total], NULL, &reader_start, NULL);
        } else {
          pthread_create(&pt_ids[total], NULL, &writer_start, NULL);
        }
        total++;
      }
    }
    random_wait(RUN_WAIT_MIN, RUN_WAIT_MAX);
  }

  for (int i = 0; i < total; i++) {
    pthread_join(pt_ids[i], NULL);
  }

  printf("\n");
  printf("Current reader count: %d\n", reader_count);
  printf("Current writer count: %d\n", writer_count);
  printf("Current thread count: %d\n", reader_count + writer_count);
  printf("Max reader wait: %d\n", reader_max);
  printf("Max writer wait: %d\n", writer_max);

  return 0;
}
