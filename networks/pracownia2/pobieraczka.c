#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <ctype.h>
#include "sockwrap.h"
#include <stdbool.h>

#define SERVER_IP "156.17.4.30"
#define MAXMSG 1800
#define PACKETS_EACH_TURN 20
#define PACKETS_REDUNDANCY 2
char buffer[MAXMSG+1];

void reset_timeout(struct timeval * timeout) {
  timeout->tv_sec = 1;
  timeout->tv_usec = 0;
}

void prepare_connection_data(struct sockaddr_in * server_address, int server_port) {
  bzero(server_address, sizeof(*server_address));
  server_address->sin_family = AF_INET;
  server_address->sin_port   = htons(server_port);
  inet_pton(AF_INET, SERVER_IP, &server_address->sin_addr);
}

void prepare_message(char buffer[], int msg_offset, int msg_size) {
  sprintf(buffer, "GET %d %d\n", msg_offset, msg_size);
  printf("Wysylamy %s", buffer);
}

void send_packets(int sockfd, char buffer[], struct sockaddr_in * server_address, int count) {
  for (int j = 0; j < count; j++) {
    Sendto(sockfd, buffer, strlen(buffer), 0, server_address, sizeof(*server_address));
  }
}

int main(int argc, char ** argv) {
  if (argc != 4) {
    printf("Run: ./pobieraczka <PORT> <NAZWA_PLIKU> <ROZMIAR>\n");
    exit(1);
  }

  int msg_chunk_size = 1000;

  int download_size = atoi(argv[3]);
  int chunks = (download_size / msg_chunk_size);
  if (download_size % msg_chunk_size != 0) {
    chunks++;
  }
  int last_chunk_size = (download_size % msg_chunk_size == 0) ? msg_chunk_size : (download_size % msg_chunk_size);
  int received[chunks];
  for (int i = 0; i < chunks; i++) {
    received[chunks] = false;
  }
  char data_tab[chunks][msg_chunk_size];


  int sockfd = Socket(AF_INET, SOCK_DGRAM, 0);

  // Struktura opisująca IP i port serwera
  int server_port = atoi(argv[1]);
  struct sockaddr_in server_address;
  prepare_connection_data(&server_address, server_port);

  // Wysyłanie jakiegos napisu do serwera
  char sending_buffer[MAXMSG];

  int packets_each_turn;
  if (PACKETS_EACH_TURN > chunks) {
    packets_each_turn = chunks;
  } else {
    packets_each_turn = PACKETS_EACH_TURN;
  }
  printf("Packets each turn: %d\n", packets_each_turn);


  char receiving_buffer[MAXMSG];

  int chunks_remaining = chunks;
  int current_chunk = 0;
  while (chunks_remaining > 0) {
    int packets_to_send = packets_each_turn;
    packets_to_send %= chunks_remaining;

    while (packets_to_send > 0) {
      if (!received[current_chunk]) {
	int msg_offset = current_chunk * msg_chunk_size;
	int msg_size = (current_chunk == chunks - 1) ? (download_size % 1000) : 1000;
	prepare_message(sending_buffer, msg_offset, msg_size);

	send_packets(sockfd, &sending_buffer, &server_address, PACKETS_REDUNDANCY);

	packets_to_send--;
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
    struct timeval timeout;
    reset_timeout(&timeout);

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

	if (!received[downloaded_chunk]) {
	  char * newline_pos = strchr(receiving_buffer, '\n');
	  newline_pos++;

	  memcpy(data_tab[downloaded_chunk], newline_pos, data_length);

	  received[downloaded_chunk] = true;
	  chunks_remaining--;
	}

	got_packets++;
      } else {
	select_finished = true;
      }
    }
  }

  // Saving to file
  FILE * target_file;
  target_file = fopen(argv[2], "wb");

  fwrite(data_tab, msg_chunk_size, chunks - 1, target_file);

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

