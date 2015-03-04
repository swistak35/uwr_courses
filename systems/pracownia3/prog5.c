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

#define CONSUMERS_COUNT 10
#define PRODUCERS_COUNT 5
#define MAX_ITEMS  4800
#define TOTAL_RESOURCES 50000
#define CONSUMERS_SPEED 5
#define PRODUCERS_SPEED 1

#define NS_IN_US 1000

struct tinfo {
  pthread_t id;
  int num;
};

int resources = 0;
int upper_bound_blocked = 0;
int lower_bound_blocked = 0;
pthread_mutex_t mx_res;
pthread_cond_t free_slot, item_ready;

void random_wait(int min, int max) {
  struct timespec sleep_time;
  sleep_time.tv_sec = 0;
  sleep_time.tv_nsec = ((rand() % (max - min)) + min) * NS_IN_US;
  nanosleep(&sleep_time, NULL);
}

void * producer(void * arg) {
  struct tinfo * current_thread = arg;
  printf("Mam do wyprodukowania %d danych.\n", current_thread->num);
  int data = current_thread->num;

  while (data > 0) {
    pthread_mutex_lock(&mx_res);
    while (resources == MAX_ITEMS) {
      upper_bound_blocked++;
      pthread_cond_wait(&free_slot, &mx_res);
    }
    resources++;
    pthread_cond_signal(&item_ready);
    pthread_mutex_unlock(&mx_res);

    data--;
    random_wait(PRODUCERS_SPEED*10, PRODUCERS_SPEED*20);
  }
  return 0;
}

void * consumer(void * arg) {
  struct tinfo * current_thread = arg;
  printf("Mam do skonsumowania %d danych.\n", current_thread->num);
  int data = current_thread->num;

  while (data > 0) {
    pthread_mutex_lock(&mx_res);
    while (resources == 0) {
      lower_bound_blocked++;
      pthread_cond_wait(&item_ready, &mx_res);
    }
    resources--;
    pthread_cond_signal(&free_slot);
    pthread_mutex_unlock(&mx_res);

    data--;
    random_wait(CONSUMERS_SPEED*10, CONSUMERS_SPEED*20);
  }
  return 0;
}


int main(void) {
  struct tinfo producers[PRODUCERS_COUNT];
  struct tinfo consumers[CONSUMERS_COUNT];

  int tmp;
  srand(time(NULL));

  printf("Losowanie dóbr...\n");
  for (int i = 0; i < CONSUMERS_COUNT; i++) {
    consumers[i].num = 0;
  }
  for (int i = 0; i < PRODUCERS_COUNT; i++) {
    producers[i].num = 0;
  }
  for (int i = 0; i < TOTAL_RESOURCES; i++) {
    tmp = rand() % CONSUMERS_COUNT;
    consumers[tmp].num++;

    tmp = rand() % PRODUCERS_COUNT;
    producers[tmp].num++;
  }

  for (int i = 0; i < CONSUMERS_COUNT; i++) {
    printf("%d ", consumers[i].num);
  }
  printf("\n");
  for (int i = 0; i < PRODUCERS_COUNT; i++) {
    printf("%d ", producers[i].num);
  }
  printf("\n");


  // start threads
  printf("Start!\n");
  for (int i = 0; i < CONSUMERS_COUNT; i++) {
    pthread_create(&consumers[i].id, NULL, &consumer, &consumers[i]);
  }

  for (int i = 0; i < PRODUCERS_COUNT; i++) {
    pthread_create(&producers[i].id, NULL, &producer, &producers[i]);
  }

  // stop threads
  for (int i = 0; i < CONSUMERS_COUNT; i++) {
    pthread_join(consumers[i].id, NULL);
  }
  for (int i = 0; i < PRODUCERS_COUNT; i++) {
    pthread_join(producers[i].id, NULL);
  }

  printf("Upper %d\n", upper_bound_blocked);
  printf("Lower %d\n", lower_bound_blocked);

  return 0;
}
