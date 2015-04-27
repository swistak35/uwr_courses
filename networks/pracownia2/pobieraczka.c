#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <ctype.h>
#include "sockwrap.h"
#include <stdbool.h>

#define SERVER_HOST "aisd.ii.uni.wroc.pl"
#define MAXMSG 1800
#define PACKETS_EACH_TURN 20
#define PACKETS_REDUNDANCY 2
char buffer[MAXMSG+1];

int main(int argc, char ** argv) {
  if (argc != 4) {
    printf("Run: ./pobieraczka <PORT> <NAZWA_PLIKU> <ROZMIAR>\n");
    exit(1);
  }

  int server_port = atoi(argv[1]);
  int download_size = atoi(argv[3]);
  int chunks = (download_size / 1000);
  if (download_size % 1000 != 0) {
    chunks++;
  }
  int sent[chunks];
  for (int i = 0; i < chunks; i++) {
    sent[chunks] = 0;
  }
  char data_tab[chunks][1000];


  int sockfd = Socket(AF_INET, SOCK_DGRAM, 0);

  // Struktura opisująca IP i port serwera
  struct sockaddr_in server_address;
  bzero (&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(server_port);
  inet_pton(AF_INET, "156.17.4.30", &server_address.sin_addr);

  // Wysyłanie jakiegos napisu do serwera
  char sending_buffer[MAXMSG];
  char number_buffer[32];

  int packets_each_turn;
  if (PACKETS_EACH_TURN > chunks) {
    packets_each_turn = chunks;
  } else {
    packets_each_turn = PACKETS_EACH_TURN;
  }
  printf("Packets each turn: %d\n", packets_each_turn);

  struct timeval timeout;
  timeout.tv_sec = 1;
  timeout.tv_usec = 0;

  char receiving_buffer[MAXMSG];

  /* int it = 1; */
  int current_chunk = 0;
  while (true) {
    bool should_we_break = true;

    for (int j = 0; j < chunks; j++) {
      if (sent[j] >= 0) {
	should_we_break = false;
	break;
      }
    }
    printf("We should break: %d\n", should_we_break);
    if (should_we_break) {
      break;
    }

    int to_send = packets_each_turn;
    while (to_send > 0) {
      if (sent[current_chunk] >= 0) {
	strcpy(sending_buffer, "GET ");
	sprintf(number_buffer, "%d ", current_chunk * 1000);
	strcat(sending_buffer, number_buffer);
	if (current_chunk == chunks - 1) {
	  int last_chunk = download_size % 1000;
	  sprintf(number_buffer, "%d", last_chunk);
	  strcat(sending_buffer, number_buffer);
	} else {
	  strcat(sending_buffer, "1000");
	}

	/* strcpy(sending_buffer, "GET 0 1000\n"); */
	printf("Wysylamy '%s'\n", sending_buffer);

	strcat(sending_buffer, "\n");

	for (int j = 0; j < PACKETS_REDUNDANCY; j++) {
	  Sendto(sockfd, sending_buffer, strlen(sending_buffer), 0, &server_address, sizeof(server_address));
	}
	sent[current_chunk]++;
	to_send--;
      }
      current_chunk++;
      current_chunk %= chunks;
    }
    printf("Odbieranie...\n");

    // odbieranie
    fd_set descriptors;
    FD_ZERO(&descriptors);
    FD_SET(sockfd, &descriptors);

    bool select_finished = false;
    int got_packets = 0;
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    while (!select_finished && (got_packets < 10)) {
      int ready = Select(sockfd+1, &descriptors, NULL, NULL, &timeout);

      if (ready != 0) {
	int n = Recvfrom(sockfd, receiving_buffer, MAXMSG, 0, NULL, NULL);
	receiving_buffer[n] = 0;
	// sprawdzac czy n jest odpowiednio duze
	printf("server reply: %s\n", receiving_buffer);
	int data_length, data_offset;
	sscanf(receiving_buffer, "DATA %d %d\n", &data_offset, &data_length);
	int downloaded_chunk = data_offset / 1000;
	printf("Dostalismy dane o %d (%d, %d)\n", downloaded_chunk, data_offset, data_length);

	char * newline_pos = strchr(receiving_buffer, '\n');
	newline_pos++;

	memcpy(data_tab[downloaded_chunk], newline_pos, data_length);

	sent[downloaded_chunk] = -1;
	got_packets++;
      } else {
	select_finished = true;
      }
    }
  }

  FILE * target_file;
  target_file = fopen("test.bin", "wb");

  fwrite(data_tab, 1000, chunks - 1, target_file);

  if (download_size % 1000 == 0) {
    fwrite(data_tab[chunks-1], 1000, 1, target_file);
  } else {
    fwrite(data_tab[chunks-1], download_size % 1000, 1, target_file);
  }
  fclose(target_file);
  /* for (int i = 0; i < 10; i++) { */
  /*   Sendto(sockfd, sending_buffer, strlen(sending_buffer), 0, &server_address, sizeof(server_address)); */
  /* } */

  // Otrzymywanie informacji do serwera (prawdopodobnie od serwera,
  // nie sprawdzamy tego!)
}

