#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <semaphore.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
#include <string.h>

#define CLIENTS 30
#define MAX_EATING_TIME 9
#define CLIENT_RESPAWN_TIME 2

typedef struct restaurant {
  sem_t mutex;
  sem_t queue;

  int waiting;
  int eating;
  bool must_wait;
} restaurant_t;

void client(int eat_time, restaurant_t * rest) {
  int newcomers;

  sem_wait(&(rest->mutex));
  if (rest->must_wait) {
    rest->waiting++;
    sem_post(&(rest->mutex));
    printf("Przychodze do restauracji, musze czekac, jestem %d w kolejce, a je jeszcze %d osoby\n", rest->waiting, rest->eating);
    sem_wait(&(rest->queue));
    printf("Siadlem do jedzenia\n");
  } else {
    printf("Przychodze do restauracji, siadam, jest nas juz %d\n", rest->eating);
    rest->eating++;
    if (rest->eating == 5) {
      rest->must_wait = true;
    }
    sem_post(&(rest->mutex));
  }

  sleep(eat_time);

  sem_wait(&(rest->mutex));
    rest->eating--;
    if (rest->eating == 0) {
      newcomers = (rest->waiting > 5 ? 5 : rest->waiting);
      rest->waiting -= newcomers;
      rest->eating += newcomers;
      if (rest->eating == 5) {
        rest->must_wait = true;
      }
      for (int i = 0; i < newcomers; i++) {
        sem_post(&(rest->queue));
      }
    }
    printf("Wychodze\n");

  sem_post(&(rest->mutex));
}

int main(void) {
  restaurant_t * rest;

  int fd = shm_open("/ramen", O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  ftruncate(fd, sizeof(restaurant_t));
  rest = (restaurant_t *) mmap(NULL, sizeof(restaurant_t), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

  rest->waiting = 0;
  rest->eating = 0;
  rest->must_wait = false;

  sem_init(&rest->mutex, 1, 1);
  sem_init(&rest->queue, 1, 0);

  unsigned int seed = 123;
  int pause_time, eat_time;

  for (int i = 0; i < CLIENTS; i++) {
    pause_time = 1 + (rand_r(&seed) % CLIENT_RESPAWN_TIME);
    eat_time = 3 + (rand_r(&seed) % MAX_EATING_TIME);
    sleep(pause_time);
    if (fork() == 0) {
      client(eat_time, rest);
      return 0;
    }
  }

  int status;
  for (int i = 0; i < CLIENTS; i++) {
    wait(&status);
  }


  munmap(rest, sizeof(restaurant_t));
  shm_unlink("/ramen");
  close(fd);

  return 0;
}
