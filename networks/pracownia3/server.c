#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/stat.h>
#include "sockwrap.h"

#define DEBUG 1

#define dprintf(...) \
    do { if (DEBUG) printf(__VA_ARGS__); } while (0)
#define MAX_QUEUE 100
#define HTTP_VERSIONS_AMOUNT 1
#define HTTP_METHODS_AMOUNT 1
#define HTTP_TYPES_AMOUNT 8
#define STRING_LENGTH 128

char buffer[4096];
int buffer_len;
int buffer_cnt;

char http_status_codes[600][STRING_LENGTH];
char http_versions[HTTP_VERSIONS_AMOUNT][STRING_LENGTH];
char http_methods[HTTP_METHODS_AMOUNT][STRING_LENGTH];
char http_content_types[HTTP_TYPES_AMOUNT][2][STRING_LENGTH];
char http_default_content_type[STRING_LENGTH];

struct http_request {
  char type;
  char http_version;
  char path[STRING_LENGTH];
  char host[STRING_LENGTH];

  char param_host[STRING_LENGTH];
  char param_connection;
  char param_accept[STRING_LENGTH];
  int param_content_length;
};

struct http_response {
  char http_version;
  int status_code;
  int file_size;
  char filepath[STRING_LENGTH];
  FILE * source_file;

  char param_content_type[STRING_LENGTH];
  char param_location[STRING_LENGTH];
};

void load_data();
void load_mime_type(char filepath[], char content_type[]);
bool is_path_requested_unsafe(char filepath[], char host[]);
void set_error_filepath(char filepath[], int status_code);
void set_param_location(char str[], struct http_request * req);
int  choose_status_code(struct http_request * req, struct http_response * res);
void print_response(char headers[]);
void parse_http_first_line(struct http_request * req, char recv_buffer[]);
void parse_http_other_line(struct http_request * req, char recv_buffer[]);
int  get_http_ver_index(int index);
bool prepare_http_response(struct http_request * req, struct http_response * res);
int  build_response_packet(struct http_response * res, char ** packet_ptr);
int  find_matching_string(char db[][STRING_LENGTH], char str[], int count);
int  bind_server(struct sockaddr_in * server_address, int server_port);
bool load_request(int conn_sockfd, struct http_request * http_req);
void InitBuffer();
int  ReadBufferedByte(int fd, char* c, struct timeval* tv);
int  ReadLine(int fd, char* buff, int len, struct timeval * tv);

char server_dir[STRING_LENGTH];

int main(int argc, char ** argv) {
  // Parsing arguments
  if (argc != 3) {
    exit(EXIT_FAILURE);
  }
  int server_port = atoi(argv[1]);
  strcpy(server_dir, argv[2]);

  // Loading initial data
  load_data();

  // Set up server
  struct sockaddr_in server_address;
  int sockfd = bind_server(&server_address, server_port);

  while (1) {
    // Accept client connections
    struct sockaddr_in client_address;
    char ip_address[20];
    socklen_t len = sizeof(client_address);
    int conn_sockfd = Accept (sockfd, &client_address, &len);
    inet_ntop (AF_INET, &client_address.sin_addr, ip_address, sizeof(ip_address));
    printf("=== New client %s:%d\n\n", ip_address, ntohs(client_address.sin_port));
    InitBuffer();

    bool connection_alive = true;
    while (connection_alive) {
      struct http_request http_req;
      bzero(&http_req, sizeof(http_req));

      if (load_request(conn_sockfd, &http_req)) {
	// Prepare and send response
	struct http_response http_res;
	char * packet;
	int packet_size;
	prepare_http_response(&http_req, &http_res);
	packet_size = build_response_packet(&http_res, &packet);
	Send(conn_sockfd, packet, packet_size, 0);
	free(packet);

	if (http_req.param_connection) {
	  connection_alive = false;
	}
      } else {
	connection_alive = false;
      }
    }

    Close(conn_sockfd);

    printf("=== Disconnected\n\n");
  }
}

void InitBuffer() {
  buffer_len = 0;
  buffer_cnt = 0;
}

int ReadBufferedByte (int fd, char* c, struct timeval* tv)
{
	if (buffer_cnt == buffer_len) {
		// musimy faktycznie wczytac dane z gniazda do bufora,
		// czekamy co najwyzej tv na jakiekolwiek dane
		/* printf ("DEBUG: no data in buffer; waiting...\n"); */
		buffer_cnt = 0;
		fd_set descriptors;
		FD_ZERO (&descriptors);
		FD_SET (fd,&descriptors);
		int ready = Select(fd+1, &descriptors, NULL, NULL, tv);
		/* printf ("DEBUG: current value of tv = %.3f\n", tv->tv_sec + tv->tv_usec * 1.0 / 1000000); */
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
int ReadLine (int fd, char* buff, int len, struct timeval * tv)
{
	int n = 0;
	char c;
	int cnt;
	while ( (cnt = ReadBufferedByte (fd, &c, tv)) ) {
		if (tv->tv_sec == 0 && tv->tv_usec == 0) {
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

void load_data() {
  bzero(&http_status_codes, sizeof(http_status_codes));
  strcpy(http_status_codes[200], "OK");
  strcpy(http_status_codes[301], "Moved Permanently");
  strcpy(http_status_codes[403], "Forbidden");
  strcpy(http_status_codes[404], "Not Found");
  strcpy(http_status_codes[501], "Not Implemented");

  bzero(&http_versions, sizeof(http_versions));
  strcpy(http_versions[0], "HTTP/1.1");

  bzero(&http_methods, sizeof(http_methods));
  strcpy(http_methods[0], "GET");

  bzero(&http_default_content_type, sizeof(http_default_content_type));
  strcpy(http_default_content_type, "application/octet-stream");

  bzero(&http_content_types, sizeof(http_content_types));
  strcpy(http_content_types[1][0], ".css");
  strcpy(http_content_types[1][1], "text/css");
  strcpy(http_content_types[2][0], ".png");
  strcpy(http_content_types[2][1], "image/png");
  strcpy(http_content_types[3][0], ".jpg");
  strcpy(http_content_types[3][1], "image/jpg");
  strcpy(http_content_types[4][0], ".jpeg");
  strcpy(http_content_types[4][1], "image/jpeg");
  strcpy(http_content_types[5][0], ".pdf");
  strcpy(http_content_types[5][1], "application/pdf");
  strcpy(http_content_types[6][0], ".txt");
  strcpy(http_content_types[6][1], "text/plain");
  strcpy(http_content_types[7][0], ".html");
  strcpy(http_content_types[7][1], "text/html");
}

void load_mime_type(char filepath[], char content_type[]) {
  char extension[STRING_LENGTH];
  char * last_dot = strrchr(filepath, '.');

  if (last_dot == NULL) {
    strcpy(content_type, http_default_content_type);
    return;
  }

  strcpy(extension, last_dot);

  for (int i = 1; i < HTTP_TYPES_AMOUNT; i++) {
    if (strcmp(extension, http_content_types[i][0]) == 0) {
      strcpy(content_type, http_content_types[i][1]);
      return;
    }
  }

  strcpy(content_type, http_default_content_type);
}

bool is_path_requested_unsafe(char filepath[], char host[]) {
  char real_filepath[STRING_LENGTH];
  char cwd[STRING_LENGTH];
  char domain[STRING_LENGTH];
  realpath(filepath, real_filepath);
  getcwd(cwd, STRING_LENGTH);
  /* strcat(cwd, "/strony_www/"); */
  strcat(cwd, "/");
  strcat(cwd, server_dir);

  int i = 0;
  while (i < STRING_LENGTH) {
    if (cwd[i] != real_filepath[i]) {
      break;
    }
    i++;
  }
  strcpy(domain, real_filepath + i);
  char * slash_ptr = strchr(domain, '/');
  if (slash_ptr == NULL) {
    return false;
  }
  *slash_ptr = '\0';

  return (strcmp(domain, host) != 0);
}

void set_error_filepath(char filepath[], int status_code) {
  sprintf(filepath, "status_codes/%d.html", status_code);
}

void set_param_location(char str[], struct http_request * req) {
  sprintf(str, "http://%s%s", req->param_host, req->path);
  int len = strlen(str);
  if (str[len-1] != '/') {
    strcat(str, "/");
  }
  strcat(str, "index.html");
}

int choose_status_code(struct http_request * req, struct http_response * res) {
  if (req->type == 0) {
    return 501;
  }

  if (req->http_version == 0) {
    return 501;
  }

  dprintf("Plik: %s\n", res->filepath);

  FILE * source_file = fopen(res->filepath, "rb");

  if (source_file == NULL) {
    return 404;
  }

  fclose(source_file);

  if (is_path_requested_unsafe(res->filepath, req->host)) {
    return 403;
  }

  struct stat file_stat;
  stat(res->filepath, &file_stat);

  if (S_ISDIR(file_stat.st_mode)) {
    return 301;
  } else if (S_ISREG(file_stat.st_mode)) {
    return 200;
  } else {
    return 501;
  }
}

void print_response(char headers[]) {
  char * pch;
  pch = strtok(headers, "\n");
  while (pch != NULL) {
    printf("--> %s\n", pch);
    pch = strtok(NULL, "\n");
  }
  printf("\n");
}

void parse_http_first_line(struct http_request * req, char recv_buffer[]) {
  char type[16];
  char path[64];
  char version[16];
  sscanf(recv_buffer, "%s %s %s\n", type, path, version);
  printf("Type: `%s`\n", type);
  printf("Path: `%s`\n", path);
  printf("Version: `%s`\n", version);

  // Parse HTTP method
  req->type = find_matching_string(http_methods, type, HTTP_METHODS_AMOUNT);

  // Parse HTTP version
  req->http_version = find_matching_string(http_versions, version, HTTP_VERSIONS_AMOUNT);

  // Parse path
  strcpy(req->path, path);
}

void parse_http_other_line(struct http_request * req, char recv_buffer[]) {
  char param_name[STRING_LENGTH];
  char param_value[STRING_LENGTH];
  sscanf(recv_buffer, "%s %s\n", param_name, param_value);

  if (strcmp(param_name, "Host:") == 0) {
    strcpy(req->param_host, param_value);
    strcpy(req->host, param_value);

    char * colon_position = strchr(req->host, ':');
    if (colon_position != NULL) {
      *colon_position = '\0';
    }
  }

  if (strcmp(param_name, "Content-Length:") == 0) {
    req->param_content_length = atoi(param_value);
    dprintf("Content-Length: `%d`\n", req->param_content_length);
  }
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

int get_http_ver_index(int index) {
  if (index == 0) {
    return 0;
  } else {
    return (index - 1);
  }
}

bool prepare_http_response(struct http_request * req, struct http_response * res) {
  bzero(res, sizeof(*res));

  sprintf(res->filepath, "%s%s%s", server_dir, req->host, req->path);

  res->status_code = choose_status_code(req, res);
  res->http_version = req->http_version;

  if (res->status_code == 501) {
    set_error_filepath(res->filepath, res->status_code);
  } else if (res->status_code == 403) {
    set_error_filepath(res->filepath, res->status_code);
  } else if (res->status_code == 404) {
    set_error_filepath(res->filepath, res->status_code);
  } else if (res->status_code == 301) {
    set_param_location(res->param_location, req);
  }

  res->source_file = fopen(res->filepath, "r");
  fseek(res->source_file, 0, SEEK_END);
  res->file_size = ftell(res->source_file);
  fclose(res->source_file);

  load_mime_type(res->filepath, res->param_content_type);

  return true;
}


int build_response_packet(struct http_response * res, char ** packet_ptr) {
  char http_headers[1024];
  char tmp[STRING_LENGTH];

  sprintf(http_headers, "%s %d %s\n",
      http_versions[get_http_ver_index(res->http_version)],
      res->status_code,
      http_status_codes[res->status_code]);

  if (strcmp(res->param_content_type, "")) {
    sprintf(tmp, "Content-Type: %s\n", res->param_content_type);
    strcat(http_headers, tmp);
  }

  if (res->file_size > 0) {
    sprintf(tmp, "Content-Length: %d\n", res->file_size);
    strcat(http_headers, tmp);
  }

  if (strcmp(res->param_location, "")) {
    sprintf(tmp, "Location: %s\n", res->param_location);
    strcat(http_headers, tmp);
  }

  strcat(http_headers, "\n");
  int headers_size = strlen(http_headers);

  *packet_ptr = malloc(headers_size + res->file_size);

  memcpy(*packet_ptr, http_headers, headers_size);

  if (res->file_size > 0) {
    FILE * source_file;
    source_file = fopen(res->filepath, "r");
    fread((*packet_ptr) + headers_size, res->file_size, 1, source_file);
    fclose(source_file);
  }

  print_response(http_headers);

  return (headers_size + res->file_size);
}

int find_matching_string(char db[][STRING_LENGTH], char str[], int count) {
  for (int i = 0; i < count; i++) {
    if (strcmp(db[i], str) == 0) {
      return (i+1);
    }
  }
  return 0;
}

bool load_request(int conn_sockfd, struct http_request * http_req) {
  int maxsize = STRING_LENGTH;
  char recv_buffer[maxsize+1];
  int n;

  struct timeval tv;
  tv.tv_sec  = 1;
  tv.tv_usec = 0;

  int it = 0;
  while ((n = ReadLine (conn_sockfd, recv_buffer, maxsize, &tv))) {
    if (n < 0) {
      break;
    }
    recv_buffer[n] = 0;

    printf("<-- %s", recv_buffer);

    if (it == 0) {
      parse_http_first_line(http_req, recv_buffer);
    } else {
      parse_http_other_line(http_req, recv_buffer);
    }
    it++;

    if (strcmp(recv_buffer, "\r\n") == 0) {
      if (http_req->param_content_length > 0) {
	char c;
	for (int i = 0; i < http_req->param_content_length; i++) {
	  ReadBufferedByte(conn_sockfd, &c, &tv);
	}
      }
      return true;
    }
  }
  printf("\n");
  return false;
}
