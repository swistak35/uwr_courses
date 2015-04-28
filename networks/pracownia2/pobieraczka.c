#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <ctype.h>
#include "sockwrap.h"
#include <stdbool.h>

#define SERVER_IP "156.17.4.30"
#define MAXMSG 1800
#define MSG_CHUNK_SIZE 1000
#define PACKETS_EACH_TURN 40
#define PACKETS_REDUNDANCY 1
#define GOT_PACKETS_BOUND 6
#define MILLISECONDS_TIMEOUT 30
#define DEBUG 0

char buffer[MAXMSG+1];

#define dprintf(...) \
    do { if (DEBUG) printf(__VA_ARGS__); } while (0)


bool receive_message(int sockfd, char receiving_buffer[], int * data_offset, int * data_length) {
  int n = Recvfrom(sockfd, receiving_buffer, MAXMSG, 0, NULL, NULL);
  receiving_buffer[n] = 0;

  // sprawdzac czy n jest odpowiednio duze
  sscanf(receiving_buffer, "DATA %d %d\n", data_offset, data_length);
  return true;
}

void copy_message(char receiving_buffer[], char data_tab[][MSG_CHUNK_SIZE], int downloaded_chunk, int data_length) {
  char * newline_pos = strchr(receiving_buffer, '\n');
  newline_pos++;

  memcpy(data_tab[downloaded_chunk], newline_pos, data_length);
}

void write_data_to_file(char filename[], char data_tab[][MSG_CHUNK_SIZE], int chunks, int last_chunk_size) {
  FILE * target_file;
  target_file = fopen(filename, "wb");

  fwrite(data_tab, MSG_CHUNK_SIZE, chunks - 1, target_file);
  fwrite(data_tab[chunks-1], last_chunk_size, 1, target_file);

  fclose(target_file);
}

int upper_bound(int number, int bound) {
  return ((number > bound) ? bound : number);
}

int lower_bound(int number, int bound) {
  return ((number < bound) ? bound : number);
}

void reset_timeout(struct timeval * timeout) {
  timeout->tv_sec = 0;
  timeout->tv_usec = MILLISECONDS_TIMEOUT * 1000;
}

int connect_socket(struct sockaddr_in * srv_addr, int server_port) {
  bzero(srv_addr, sizeof(*srv_addr));
  srv_addr->sin_family = AF_INET;
  srv_addr->sin_port   = htons(server_port);
  inet_pton(AF_INET, SERVER_IP, &srv_addr->sin_addr);

  int sockfd = Socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  Connect(sockfd, srv_addr, sizeof(*srv_addr));

  /* bzero(cli_addr, sizeof(*cli_addr)); */
  /* socklen_t slen = sizeof(*cli_addr); */
  /* Getsockname(sockfd, cli_addr, &slen); */

  return sockfd;
}

void prepare_message(char buffer[], int msg_offset, int msg_size) {
  sprintf(buffer, "GET %d %d\n", msg_offset, msg_size);
  dprintf("Wysylamy %s", buffer);
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

  int download_size = atoi(argv[3]);
  int chunks = (download_size / MSG_CHUNK_SIZE);
  if (download_size % MSG_CHUNK_SIZE != 0) {
    chunks++;
  }
  int last_chunk_size = (download_size % MSG_CHUNK_SIZE == 0) ? MSG_CHUNK_SIZE : (download_size % MSG_CHUNK_SIZE);
  bool received[chunks];
  for (int i = 0; i < chunks; i++) {
    received[i] = false;
  }
  char data_tab[chunks][MSG_CHUNK_SIZE];

  // Struktura opisująca IP i port serwera
  int server_port = atoi(argv[1]);
  struct sockaddr_in server_address;
  int sockfd = connect_socket(&server_address, server_port);


  // Bufory
  char sending_buffer[MAXMSG];
  char receiving_buffer[MAXMSG];

  int chunks_remaining = chunks;
  int current_chunk = 0;
  int got_packets_bound = 1;
  while (chunks_remaining > 0) {
    printf("Remaining: %d/%d\n", chunks_remaining, chunks);
    int packets_to_send = upper_bound(PACKETS_EACH_TURN, chunks_remaining);

    while (packets_to_send > 0) {
      if (!received[current_chunk]) {
	int msg_offset = current_chunk * MSG_CHUNK_SIZE;
	int msg_size = (current_chunk == chunks - 1) ? last_chunk_size : MSG_CHUNK_SIZE;
	prepare_message(sending_buffer, msg_offset, msg_size);

	send_packets(sockfd, sending_buffer, &server_address, PACKETS_REDUNDANCY);

	packets_to_send--;
      }

      current_chunk++;
      current_chunk %= chunks;
    }

    // odbieranie
    dprintf("Odbieranie...\n");
    fd_set descriptors;
    FD_ZERO(&descriptors);
    FD_SET(sockfd, &descriptors);

    struct timeval timeout;
    reset_timeout(&timeout);

    int got_packets = 0;
    while (got_packets < got_packets_bound) {
      int ready = Select(sockfd+1, &descriptors, NULL, NULL, &timeout);

      if (ready == 0) {
	break;
      } else {
	int data_length, data_offset, downloaded_chunk;

	receive_message(sockfd, receiving_buffer, &data_offset, &data_length);

	downloaded_chunk = data_offset / MSG_CHUNK_SIZE;
	dprintf("Dostalismy dane o %d (%d, %d)\n", downloaded_chunk, data_offset, data_length);
	dprintf("%s", receiving_buffer);

	if (!received[downloaded_chunk]) {
	  copy_message(receiving_buffer, data_tab, downloaded_chunk, data_length);
	  received[downloaded_chunk] = true;
	  chunks_remaining--;
	}

	got_packets++;
      }
    }
    dprintf("Packets: %d/%d\n", got_packets, got_packets_bound);

    if (got_packets == got_packets_bound) {
      got_packets_bound = upper_bound(got_packets_bound + 1, GOT_PACKETS_BOUND);
    } else {
      got_packets_bound = lower_bound(got_packets, 1);
    }
  }

  // Saving to file
  write_data_to_file(argv[2], data_tab, chunks, last_chunk_size);

  return 0;
}

