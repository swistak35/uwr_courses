#include <stdio.h>
#include <stdlib.h>
#include <semaphore.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <fcntl.h>
#include <string.h>

#define HORSES_AMOUNT 5
#define RACE_TIME_LIMIT 5
#define RACES 5

typedef struct barrier {
  int shm_fd;
  char shm_name[128];
  int limit;
  int current;

  sem_t mutex;

  sem_t phase1, phase2;
} barrier_t;

void barrier_init(barrier_t ** barrier_ptr, char name[], int limit) {
  int fd = shm_open("/konie", O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  ftruncate(fd, sizeof(barrier_t));
  *barrier_ptr = (barrier_t *) mmap(NULL, sizeof(barrier_t), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

  barrier_t * barrier = *barrier_ptr;

  barrier->shm_fd = fd;

  barrier->limit = limit;
  barrier->current = 0;

  sem_init(&barrier->phase1, 1, 0);
  sem_init(&barrier->phase2, 1, 1);
  sem_init(&barrier->mutex, 1, 1);

  strcpy(barrier->shm_name, name);
}

void barrier_wait(barrier_t * barrier) {
  // horses coming to 1 barrier
  sem_wait(&(barrier->mutex));
      barrier->current++;
      if (barrier->current == barrier->limit) {
        sem_wait(&(barrier->phase2));
        sem_post(&(barrier->phase1));
      }
  sem_post(&(barrier->mutex));

  sem_wait(&(barrier->phase1));
  sem_post(&(barrier->phase1));


  sem_wait(&(barrier->mutex));
      barrier->current--;
      if (barrier->current == 0) {
        sem_wait(&(barrier->phase1));
        sem_post(&(barrier->phase2));
      }
  sem_post(&(barrier->mutex));

  sem_wait(&(barrier->phase2));
  sem_post(&(barrier->phase2));
}

void barrier_destroy(barrier_t * barrier) {
  sem_destroy(&barrier->phase1);
  sem_destroy(&barrier->phase2);
  sem_destroy(&barrier->mutex);

  int shm_fd = barrier->shm_fd;
  char shm_name[128];

  strcpy(shm_name, barrier->shm_name);

  munmap(barrier, sizeof(barrier_t));
  shm_unlink(shm_name);
  close(shm_fd);
}


void horse(int x, unsigned int seed, barrier_t * race) {
  int race_time;

  for (int i = 0; i < RACES; i++) {
    race_time = 1 + (rand_r(&seed) % RACE_TIME_LIMIT);
    printf("Kon %d bedzie biegl %d\n", x, race_time);

    // start!
    barrier_wait(race);

    // run...
    sleep(race_time);

    // finished!
    printf("%d finished!\n", x);
    barrier_wait(race);
  }
}

int main(void) {
  srand(time(NULL));

  barrier_t * race;

  barrier_init(&race, "/konie", HORSES_AMOUNT);


  for (int i = 0; i < HORSES_AMOUNT; i++) {
    if (fork() == 0) {
      horse(i, i, race);
      return 0;
    }
  }


  int status;
  for (int i = 0; i < HORSES_AMOUNT; i++) {
    wait(&status);
  }

  barrier_destroy(race);

  return 0;
}
