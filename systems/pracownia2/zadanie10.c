#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>

#define THREADS_AMOUNT 3

struct tinfo {
  pthread_t id;
  int num;
};

void * thread_start(void * arg) {
  struct tinfo * current_thread = arg;
  printf("Watek %d: Hej. Mam pthread_t = %ld, a lwp = %ld\n",
      current_thread->num,
      (long) current_thread->id,
      syscall(SYS_gettid));

  sleep(1 + current_thread->num);

  printf("Watek %d: Koniec pracy.\n", current_thread->num);

  return 0;
}

int main() {
  struct tinfo threads[THREADS_AMOUNT];

  char ps_command[100];

  printf("PID: %d\n", getpid());

  for(int t = 0; t < THREADS_AMOUNT; t++){
    threads[t].num = t;
    pthread_create(&threads[t].id, NULL, &thread_start, &threads[t]);
  }

  sprintf(ps_command, "ps -L -o pid,lwp,nlwp,cmd -p %d", getpid());
  system(ps_command);

  for (int t = 0; t < THREADS_AMOUNT; t++) {
    pthread_join(threads[t].id, NULL);
    printf("Watek %d dolaczony\n", t);
  }

  return 0;
}
