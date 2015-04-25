#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <ctype.h>
#include "sockwrap.h"
#include <stdbool.h>

#define SERVER_HOST "aisd.ii.uni.wroc.pl"
#define MAXMSG 1500
char buffer[MAXMSG+1];

int main(int argc, char ** argv) {
  if (argc != 4) {
    printf("Run: ./pobieraczka <PORT> <NAZWA_PLIKU> <ROZMIAR>\n");
    exit(1);
  }

  int server_port = atoi(argv[1]);
  /* int download_size = atoi(argv[3]); */

  int sockfd = Socket(AF_INET, SOCK_DGRAM, 0);

  // Struktura opisująca IP i port serwera
  struct sockaddr_in server_address;
  bzero (&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(server_port);
  inet_pton(AF_INET, SERVER_HOST, &server_address.sin_addr);

  // Wysyłanie jakiegos napisu do serwera
  char sending_buffer[MAXMSG];
  strcpy(sending_buffer, "GET 0 1000\n");
  Sendto(sockfd, sending_buffer, strlen(sending_buffer),
      0, &server_address, sizeof(server_address));

  // Otrzymywanie informacji do serwera (prawdopodobnie od serwera,
  // nie sprawdzamy tego!)
  char receiving_buffer[MAXMSG];
  int n = Recvfrom(sockfd, receiving_buffer, MAXMSG, 0, NULL, NULL);
  receiving_buffer[n] = 0;
  printf ("server reply: %s\n", receiving_buffer);
}

