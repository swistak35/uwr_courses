#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

extern char ** environ;

#define CWD_CHARS 128

int main(){
  char cwd[CWD_CHARS];

  int i = 0;
  setenv("envtest", "val1", 1);
  while (environ[i] != NULL) {
    printf("%s\n",environ[i]);
    i++;
  }

  pid_t child;
  getcwd(cwd, CWD_CHARS);
  printf("cwd=%s\n",cwd);
  child = fork();
  if (child == 0) {
    sleep(1);
    getcwd(cwd, CWD_CHARS);
    printf("Dziecko: envtest = %s | cwd = %s \n", getenv("envtest"), cwd);
  } else {
    setenv("envtest", "val2", 1);
    chdir("/home/swistak35");
    getcwd(cwd, CWD_CHARS);
    printf("Rodzic: envtest = %s | cwd = %s \n", getenv("envtest"), cwd);
    sleep(3);
  }
  return 0;
}
