#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include "sockwrap.h"

#define DEBUG 0

#define dprintf(...) \
    do { if (DEBUG) printf(__VA_ARGS__); } while (0)
#define MAX_QUEUE 100

#define MIN(a,b) (a)<(b)?(a):(b)

char buffer[4096];
int buffer_len;
int buffer_cnt;

char http_status_codes[600][128] = { { 0 } };
char http_versions[1][128] = { { 0 } };
char http_methods[1][128] = { { 0 } };

void load_data() {
  strcpy(http_status_codes[200], "OK");
  strcpy(http_status_codes[404], "Not Found");
  strcpy(http_versions[0], "HTTP/1.1");
  strcpy(http_methods[0], "GET");
}

struct http_request {
  char type;
  char http_version;
  char path[64];
  char param_host[64];
  char param_connection;
  char param_accept[64];
};

struct http_response {
  char http_version;
  int http_status_code;
  int file_size;
  /* char filepath[128]; */
  /* char param_content_type[128]; */
  /* char param_content_length[128]; */
};

bool is_root_path_requested(struct http_request * req) {
  if (strcmp(req->path, "/") == 0) {
    return true;
  } else {
    return false;
  }
}

void set_status_code(struct http_response * res, int code) {
  (void) res;
  (void) code;
}

bool is_path_requested_unsafe(struct http_request * req) {
  (void) req;
  return false;
}

void build_filepath(char filepath[], char host[], char filename[]) {
  sprintf(filepath, "strony_www/%s%s", host, filename);
}

bool prepare_http_response(struct http_request * req, struct http_response * res) {
  bzero(res, sizeof(*res));

  res->http_version = req->http_version;

  if (is_path_requested_unsafe(req)) {
    set_status_code(res, 403);
  } else {
    char filename[128];
    char filepath[128];

    if (is_root_path_requested(req)) {
      strcpy(filename, "/index.html");
      set_status_code(res, 301);
    } else {
      strcpy(filename, req->path);
    }

    build_filepath(filepath, req->param_host, filename);

    FILE * source_file;
    source_file = fopen(filepath, "rb");
    if (source_file == NULL) {
      set_status_code(res, 404);
    } else {
      fseek(source_file, 0, SEEK_END);
      res->file_size = ftell(source_file);
      fclose(source_file);
    }
  }

  return true;

    /* char response[1024*128]; */
    /* char content[1024*127]; */
    /* char filename[128]; */
    /* printf("Requested filetype: `%s`\n", http_req.param_accept); */
    /* FILE * source_file; */
    /* char filepath[512]; */
    /* strcpy(filepath, "strony_www/"); */
    /* strcat(filepath, http_req.param_host); */
    /* /1* strcat(filepath, "/"); *1/ */
    /* strcat(filepath, filename); */
    /* printf("Filename: %s\n", filename); */
    /* printf("Filepath: %s\n", filepath); */
    /* source_file = fopen(filepath, "r"); */

    /* int headers_size; */
    /* int file_size; */
    /* if (source_file == NULL) { */
    /*   strcat(response, "404 Not Found\n"); */
    /* } else { */
    /*   strcat(response, "200 OK\n"); */
    /*   fread(content, 1024*127, 1, source_file); */
    /*   file_size = ftell(source_file); */
    /*   printf("File size: %d\n", file_size); */
    /*   fclose(source_file); */
    /* } */

    /* if (strcmp(http_req.param_accept, "image/png") == 0) { */
    /* } else { */
    /*   char tmp[128]; */
//      if (strcmp(http_req.param_accept, "*/*") == 0) {
	/* sprintf(tmp, "Content-Type: %s\n", "text/html"); */
    /*   } else { */
	/* sprintf(tmp, "Content-Type: %s\n", http_req.param_accept); */
    /*   } */
    /*   strcat(response, tmp); */
    /* } */
    /* strcat(response, "\n"); */
    /* headers_size = strlen(response); */
    /* printf("Headers size: %d\n", headers_size); */

    /* /1* strcat(response, content); *1/ */
    /* memcpy(response + headers_size, content, file_size); */

    /* Send(conn_sockfd, response, headers_size + file_size, 0); */
}

void InitBuffer();
int ReadBufferedByte(int fd, char* c, struct timeval* tv);
int ReadLine(int fd, char* buff, int len, int timeout);

void parse_http_first_line(struct http_request * req, char recv_buffer[]) {
  dprintf("Parsing first line started...\n");

  char type[16];
  char path[64];
  char version[16];
  sscanf(recv_buffer, "%s %s %s\n", type, path, version);

  // Parse HTTP method
  if (strcmp(type, "GET") == 0) {
    req->type = 1;
  }

  // Parse HTTP version
  if (strcmp(version, "HTTP/1.1") == 0) {
    req->http_version = 1;
  }

  // Parse path
  strcpy(req->path, path);

  dprintf("Parsing first line finished...\n");
}

void parse_http_other_line(struct http_request * req, char recv_buffer[]) {
  dprintf("Parsing other line started...\n");

  char param_name[64];
  char param_value[64];
  sscanf(recv_buffer, "%s %s\n", param_name, param_value);

  if (strcmp(param_name, "Host:") == 0) {
    strcpy(req->param_host, param_value);

    char * colon_position = strchr(req->param_host, ':');
    if (colon_position != NULL) {
      printf("Znaleziono dwurkopka.\n");
      *colon_position = '\0';
    }
    // jesli port inny, to mozna walnac bledem
  }

  if (strcmp(param_name, "Accept:") == 0) {
    char * next_token = strtok(param_value, ",;");
    strcpy(req->param_accept, next_token);
  }

  dprintf("Parsing other line finished...\n");
}

int bind_server(struct sockaddr_in * server_address, int server_port) {
  int sockfd = Socket(AF_INET, SOCK_STREAM, 0);
  bzero(server_address, sizeof(*server_address));
  server_address->sin_family      = AF_INET;
  server_address->sin_port        = htons(server_port);
  server_address->sin_addr.s_addr = htonl(INADDR_ANY);
  Bind(sockfd, server_address, sizeof(*server_address));
  Listen(sockfd, MAX_QUEUE);
  return sockfd;
}

int main(int argc, char ** argv) {
  if (argc != 2) {
    exit(EXIT_FAILURE);
  }
  load_data();

  int server_port = atoi(argv[1]);
  struct sockaddr_in server_address;
  int sockfd = bind_server(&server_address, server_port);

  while (1) {
    struct sockaddr_in client_address;
    socklen_t len = sizeof(client_address);
    int conn_sockfd = Accept (sockfd, &client_address, &len);
    char ip_address[20];
    inet_ntop (AF_INET, &client_address.sin_addr, ip_address, sizeof(ip_address));
    printf ("New client %s:%d\n", ip_address, ntohs(client_address.sin_port));

    InitBuffer();
    int maxsize = 64;
    char recv_buffer[maxsize+1];
    int n;

    // Czekamy max. 7 sekund na kolejny wiersz i zapisujemy pierwsze
    // maxsize bajtow z tego wiersza do bufora recv_buffer.
    int it = 0;
    struct http_request http_req;
    while ( (n = ReadLine (conn_sockfd, recv_buffer, maxsize, 7)) ) {
      if (n < 0) {
	printf ("DEBUG: Readline error: %s\n", n == -1 ? strerror(errno) : "timeout");
	break;
      }
      recv_buffer[n] = 0;
      printf ("Chunk %d %lu ->%s<- received\n", n, sizeof(recv_buffer), recv_buffer);

      if (it == 0) {
	parse_http_first_line(&http_req, recv_buffer);
      } else {
	parse_http_other_line(&http_req, recv_buffer);
      }
      it++;

      if (strcmp(recv_buffer, "\r\n") == 0) {
	printf("wychodzimy!\n");
	break;
      }
      // Odsylamy to co zapisalismy do bufora do klienta.  Uwaga: powinnismy
      // to robic podobnie jak w programie klienta, tj. wysylac dane do
      // skutku, a nie wywolywac pojedyncza funkcje send()
    }
    struct http_response http_res;
    prepare_http_response(&http_req, &http_res);

    char response[1024*128];
    char content[1024*127];
    char filename[128];
    printf("Requested filetype: `%s`\n", http_req.param_accept);
    strcpy(response, "HTTP/1.1 ");
    if (is_root_path_requested(&http_req)) {
      strcpy(filename, "/index.html");
      // ustawic kod na 301
    } else {
      strcpy(filename, http_req.path);
    }
    FILE * source_file;
    char filepath[512];
    strcpy(filepath, "strony_www/");
    strcat(filepath, http_req.param_host);
    /* strcat(filepath, "/"); */
    strcat(filepath, filename);
    printf("Filename: %s\n", filename);
    printf("Filepath: %s\n", filepath);
    source_file = fopen(filepath, "r");

    int headers_size;
    int file_size;
    if (source_file == NULL) {
      strcat(response, "404 Not Found\n");
    } else {
      strcat(response, "200 OK\n");
      fread(content, 1024*127, 1, source_file);
      file_size = ftell(source_file);
      printf("File size: %d\n", file_size);
      fclose(source_file);
    }

    if (strcmp(http_req.param_accept, "image/png") == 0) {
    } else {
      char tmp[128];
      if (strcmp(http_req.param_accept, "*/*") == 0) {
	sprintf(tmp, "Content-Type: %s\n", "text/html");
      } else {
	sprintf(tmp, "Content-Type: %s\n", http_req.param_accept);
      }
      strcat(response, tmp);
    }
    strcat(response, "\n");
    headers_size = strlen(response);
    printf("Headers size: %d\n", headers_size);

    /* strcat(response, content); */
    memcpy(response + headers_size, content, file_size);

    Send(conn_sockfd, response, headers_size + file_size, 0);

    Close(conn_sockfd);
    printf("Disconnected\n");
  }
}

void InitBuffer() { buffer_len = buffer_cnt = 0; }

int ReadBufferedByte (int fd, char* c, struct timeval* tv)
{
	if (buffer_cnt == buffer_len) {
		// musimy faktycznie wczytac dane z gniazda do bufora,
		// czekamy co najwyzej tv na jakiekolwiek dane
		printf ("DEBUG: no data in buffer; waiting...\n");
		buffer_cnt = 0;
		fd_set descriptors;
		FD_ZERO (&descriptors);
		FD_SET (fd,&descriptors);
		int ready = Select(fd+1, &descriptors, NULL, NULL, tv);
		printf ("DEBUG: current value of tv = %.3f\n", tv->tv_sec + tv->tv_usec * 1.0 / 1000000);
		if (!ready) { return -1; }					// timeout
		buffer_len = recv(fd, buffer, 4096, 0);
		if (buffer_len <= 0) return buffer_len;		// 0 (koniec transmisji) lub -1 (blad)
	}
	*c = buffer[buffer_cnt++];
	return 1;
}

// Wczytuje caly wiersz z gniazda.
// Zapisuje pierwszych len znakow z tego wiersza do bufora buff.
// Jeśli wiersz byl dluzszy niz len znakow, dodatkowe znaki sa tracone.
//
// Zwraca dlugosc przeczytanego wiersza w przypadku powodzenia (jeśli w gnieździe
// nie ma już danych, bo klient zamknął połączenie, to zwracane jest 0).
// W przypadku błędu zwracane jest -1, w przypadku przekroczonego czasu oczekiwania
// zwracane jest -2 (czekamy timeout sekund).
//
// Wczytywanie po bajcie byloby bardzo nieefektywne, wiec wczytujemy
// wieksze kawalki do bufora roboczego, a potem przetwarzamy je po bajcie!
int ReadLine (int fd, char* buff, int len, int timeout)
{
	int n = 0;
	char c;
	int cnt;
	struct timeval tv; tv.tv_sec = timeout; tv.tv_usec = 0;
	while ( (cnt = ReadBufferedByte (fd, &c, &tv)) ) {
		if (tv.tv_sec == 0 && tv.tv_usec == 0) {
			// buff[MIN(n,len)] = 0;
			// printf ("DEBUG: timeout. Current value of the buffer: ->%s<-\n", buff);
			return -2;
		}
		if (cnt == -1) { return -1; }
		// Przeczytalismy z powodzeniem jakis bajt
		if (n < len) { buff[n++] = c; }
		if (c == '\n') { break; }
	}
	return n;
}
