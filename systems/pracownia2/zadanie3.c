#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
/* #include <string.h> */
/* #include <signal.h> */

int main(void) {
  uid_t uid, gid;
  struct passwd * passwd_user;
  struct group * group_entry;
  int groups_count = 100;
  gid_t * groups;

  uid = getuid();
  passwd_user = getpwuid(uid);

  gid = getgid();
  group_entry = getgrgid(gid);


  printf("uid=%d(%s) gid=%d(%s) grupy=",
      uid, passwd_user->pw_name,
      gid, group_entry->gr_name);

  groups = malloc(groups_count * sizeof (gid_t));
  getgrouplist(passwd_user->pw_name, passwd_user->pw_gid, groups, &groups_count);

  for (int i = 0; i < groups_count; i++) {
    group_entry = getgrgid(groups[i]);
    printf("%d(%s),", groups[i], group_entry->gr_name);
  }
  printf("\n");

  // grupy systemowe = grupy z id < 1000

  return 0;
}
