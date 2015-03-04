#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

#define BUFFER_SIZE 1024


void make_full_filepath(char filepath[], char filename[], char buffer[]) {
  // no size protection
  buffer[0] = '\0';
  strcat(buffer, filepath);
  strcat(buffer, "/");
  strcat(buffer, filename);
}

void remove_root_dir(char buffer[]) {
  char * pos = strchr(buffer, '/');
  strcpy(buffer, pos+1);
}

void scan_directory(char filepath[]) {
  DIR * dirp;
  dirp = opendir(filepath);
  if (dirp == NULL) {
    printf("Failed to open directory: %s\n", filepath);
    exit(1);
  }

  struct dirent *dentry;
  struct stat mystat;
  int permissions;
  char buffer[BUFFER_SIZE] = { 0 };
  do {
    dentry = readdir(dirp);
    if (dentry != NULL) {
      if ((strcmp(dentry->d_name, ".") != 0) && (strcmp(dentry->d_name, "..") != 0)) {
        make_full_filepath(filepath, dentry->d_name, buffer);

        stat(buffer, &mystat);

        if (S_ISDIR(mystat.st_mode)) {
          /* printf("Dir: %s %s\n", dentry->d_name, buffer); */
          scan_directory(buffer);
        } else {
          permissions = mystat.st_mode & 0777;
          remove_root_dir(buffer);
          printf("File: %s %o %d\n", buffer, permissions, (int) mystat.st_size);
        }
      }
    }
  } while (dentry != NULL);
}

int main(void) {
  scan_directory("testkatalog");

  return 0;
}
