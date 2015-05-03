#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <errno.h>
#include <stdlib.h>

#define ERROR(str) { fprintf(stderr, "%s: %s\n", str, strerror(errno)); exit(1); }

int Socket(int family, int type, int protocol);
void Bind(int fd, const struct sockaddr_in *sa, socklen_t salen);
ssize_t Recvfrom(int fd, void *ptr, size_t nbytes, int flags, struct sockaddr_in *sa, socklen_t *salenptr);
void Sendto(int fd, const void *ptr, int nbytes, int flags, const struct sockaddr_in *sa, socklen_t salen);
void Setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
int Select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout);
void Connect(int fd, const struct sockaddr_in *sa, socklen_t salen);
void Getsockname(int fd, const struct sockaddr_in *sa, socklen_t * salen);
