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

#define DEBUG 0

#define dprintf(...) \
    do { if (DEBUG) printf(__VA_ARGS__); } while (0)
#define MAX_QUEUE 100
#define HTTP_VERSIONS_AMOUNT 1
#define HTTP_METHODS_AMOUNT 1
#define HTTP_TYPES_AMOUNT 8
#define STRING_LENGTH 128

#define MIN(a,b) (a)<(b)?(a):(b)

// pilnowac polaczenia z ta sekunda i "Connection: "
// w domyslnym przypadku typ pliku to powinno byc application/octet-stream
// wysylac htmle do bledow
// napisac testy
// zabezpieczenie na 403

char buffer[4096];
int buffer_len;
int buffer_cnt;

char http_status_codes[600][STRING_LENGTH] = { { 0 } };
char http_versions[HTTP_VERSIONS_AMOUNT][STRING_LENGTH] = { { 0 } };
char http_methods[HTTP_METHODS_AMOUNT][STRING_LENGTH] = { { 0 } };
char http_content_types[HTTP_TYPES_AMOUNT][2][STRING_LENGTH] = { { { 0 } } };

void load_data() {
  strcpy(http_status_codes[200], "OK");
  strcpy(http_status_codes[301], "Moved Permanently");
  strcpy(http_status_codes[403], "Forbidden");
  strcpy(http_status_codes[404], "Not Found");
  strcpy(http_status_codes[501], "Not Implemented");

  strcpy(http_versions[0], "HTTP/1.1");

  strcpy(http_methods[0], "GET");

  strcpy(http_content_types[0][0], "");
  strcpy(http_content_types[0][1], "application/octet-stream");
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
  // sprawdzic jak wyglada jpg, jpeg i pdf
  strcpy(http_content_types[6][0], ".txt");
  strcpy(http_content_types[6][1], "text/plain");
  strcpy(http_content_types[7][0], ".html");
  strcpy(http_content_types[7][1], "text/html");
}

struct http_request {
  char type;
  char http_version;
  char path[STRING_LENGTH];
  char host[STRING_LENGTH];
  char param_host[STRING_LENGTH];
  char param_connection;
  char param_accept[STRING_LENGTH];
};

struct http_response {
  char http_version;
  int status_code;
  int file_size;
  char filepath[STRING_LENGTH];
  char param_content_type[STRING_LENGTH];
  char param_location[STRING_LENGTH];
  FILE * source_file;
};

void load_mime_type(char filepath[], char content_type[]) {
  char extension[STRING_LENGTH];
  char * last_dot = strrchr(filepath, '.');

  if (last_dot == NULL) {
    strcpy(content_type, http_content_types[0][1]);
    return;
  }

  strcpy(extension, last_dot);

  for (int i = 1; i < HTTP_TYPES_AMOUNT; i++) {
    if (strcmp(extension, http_content_types[i][0]) == 0) {
      strcpy(content_type, http_content_types[i][1]);
      return;
    }
  }

  strcpy(content_type, http_content_types[0][1]);
}

bool is_root_path_requested(struct http_request * req) {
  if (strcmp(req->path, "/") == 0) {
    return true;
  } else {
    return false;
  }
}

bool is_path_requested_unsafe(char filepath[], char host[]) {
  char real_filepath[STRING_LENGTH];
  char cwd[STRING_LENGTH];
  char domain[STRING_LENGTH];
  realpath(filepath, real_filepath);
  getcwd(cwd, STRING_LENGTH);
  strcat(cwd, "/strony_www/");
  printf("realpath: `%s`\n", real_filepath);
  printf("cwd: `%s`\n", cwd);

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
  printf("Domain: `%s`\n", domain);

  return (strcmp(domain, host) != 0);
}

void build_filepath(char filepath[], char host[], char filename[]) {
  sprintf(filepath, "strony_www/%s%s", host, filename);
  dprintf("Filepath: `%s`", filepath);
}

int get_http_ver_index(int index) {
  if (index == 0) {
    return 0;
  } else {
    return (index - 1);
  }
}

int choose_status_code(struct http_request * req, struct http_response * res) {
  if (req->type == 0) {
    return 501;
  }

  if (req->http_version == 0) {
    return 501;
  }

  res->source_file = fopen(res->filepath, "rb");

  if (res->source_file == NULL) {
    return 404;
  }

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
    // ???
    return 501;
  }
}

bool prepare_http_response(struct http_request * req, struct http_response * res) {
  bzero(res, sizeof(*res));

  /* char filepath[STRING_LENGTH]; */
  sprintf(res->filepath, "strony_www/%s%s", req->host, req->path);
  /* res->source_file = fopen(res->filepath, "rb"); */
  /* strcpy(res->filepath, filepath); */
  /* build_filepath(filepath, req->param_host, filename); */

  res->status_code = choose_status_code(req, res);
  res->http_version = req->http_version;

  if (res->status_code == 501) {
    return true;
  } else if (res->status_code == 403) {
    return true;
  } else if (res->status_code == 404) {
    return true;
  } else if (res->status_code == 301) {
    strcpy(res->param_location, "http://");
    strcat(res->param_location, req->param_host);
    strcat(res->param_location, req->path);
    strcat(res->param_location, "/index.html");
  } else if (res->status_code == 200) {
    fseek(res->source_file, 0, SEEK_END);
    res->file_size = ftell(res->source_file);
    rewind(res->source_file);
    fclose(res->source_file);

    /* char tmp[STRING_LENGTH]; */
    /* char file_cmd[STRING_LENGTH]; */
    /* sprintf(file_cmd, "file -b --mime-type %s", res->filepath); */
    /* FILE * file_cmd_pipe = popen(file_cmd, "r"); */
    /* fread(tmp, STRING_LENGTH, 1, file_cmd_pipe); */
    /* pclose(file_cmd_pipe); */
    /* printf("Ustalono type: `%s`", tmp); */
    /* for (int j = 0; j < STRING_LENGTH; j++) { */
    /*   if (tmp[j] == '\n') { */
	/* tmp[j] = '\0'; */
	/* break; */
    /*   } */
    /* } */
    load_mime_type(res->filepath, res->param_content_type);
    printf("Ustalono type: `%s`", res->param_content_type);
    /* strcpy(res->param_content_type, tmp); */
  }

  /* if (res->status_code == 501) { */
  /*   return true; */
  /* } */

  /* char filename[STRING_LENGTH]; */

  /* if (is_root_path_requested(req)) { */
  /*   strcpy(filename, "/index.html"); */
  /*   res->status_code = 301; */
  /* } else { */
  /*   strcpy(filename, req->path); */
  /* } */


  /* if (is_path_requested_unsafe(filepath)) { */
  /*   res->status_code = 403; */
  /* } else { */
  /*   FILE * source_file; */
  /*   source_file = fopen(filepath, "rb"); */

  /*   if (source_file == NULL) { */
  /*     res->status_code = 404; */
  /*   } else { */
  /*     fseek(source_file, 0, SEEK_END); */
  /*     res->file_size = ftell(source_file); */
  /*     fclose(source_file); */
  /*     dprintf("File size: %d\n", res->file_size); */

  /*     strcpy(res->filepath, filepath); */

  /*     if (res->status_code == 0) { */
	/* res->status_code = 200; */
  /*     } */
  /*   } */
  /* } */

  // if (strcmp(req->param_accept, "*/*") == 0) {
  /*   strcpy(res->param_content_type, "text/html"); */
  /* } else { */
  /*   if (is_content_type_accepted(req->param_accept)) { */
  /*     strcpy(res->param_content_type, req->param_accept); */
  /*   } else { */
  /*     res->status_code = 501; */
  /*   } */
  /* } */

  return true;
}

int build_response_packet(struct http_response * res, char ** packet_ptr) {
  char http_headers[1024];
  char tmp[STRING_LENGTH];

  sprintf(http_headers, "%s %d %s\n",
      http_versions[get_http_ver_index(res->http_version)],
      res->status_code,
      http_status_codes[res->status_code]);

  printf("jajo1 %s\n", res->filepath);

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

  printf("jajo2\n");
  strcat(http_headers, "\n");
  int headers_size = strlen(http_headers);

  *packet_ptr = malloc(headers_size + res->file_size);

  memcpy(*packet_ptr, http_headers, headers_size);

  printf("jajo3\n");
  if (res->file_size > 0) {
    FILE * source_file;
    source_file = fopen(res->filepath, "r");
    printf("jajo4\n");
    fread((*packet_ptr) + headers_size, res->file_size, 1, source_file);
    printf("jajo5 %p\n", source_file);
    fclose(source_file);
    printf("jajo6\n");
  }

  printf("Wynik: %s\n", http_headers);

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
  req->type = find_matching_string(http_methods, type, HTTP_METHODS_AMOUNT);

  // Parse HTTP version
  req->http_version = find_matching_string(http_versions, version, HTTP_VERSIONS_AMOUNT);

  // Parse path
  strcpy(req->path, path);

  dprintf("Parsing first line finished...\n");
}

void parse_http_other_line(struct http_request * req, char recv_buffer[]) {
  dprintf("Parsing other line started...\n");

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
      /* printf ("Chunk %d %lu ->%s<- received\n", n, sizeof(recv_buffer), recv_buffer); */
      printf("%s", recv_buffer);

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
    char * packet;
    int packet_size;
    printf("dupa_prepare\n");
    prepare_http_response(&http_req, &http_res);
    printf("dupa_build\n");
    packet_size = build_response_packet(&http_res, &packet);
    printf("dupa_send\n");
    Send(conn_sockfd, packet, packet_size, 0);
    free(packet);

    /* char response[1024*128]; */
    /* char content[1024*127]; */
    /* char filename[128]; */
    /* printf("Requested filetype: `%s`\n", http_req.param_accept); */
    /* strcpy(response, "HTTP/1.1 "); */
    /* if (is_root_path_requested(&http_req)) { */
    /*   strcpy(filename, "/index.html"); */
    /*   // ustawic kod na 301 */
    /* } else { */
    /*   strcpy(filename, http_req.path); */
    /* } */
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
    //  if (strcmp(http_req.param_accept, "*/*") == 0) {
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
