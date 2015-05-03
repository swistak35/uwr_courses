/*
 * Serwer echa TCP
 * 
 * Oczekuje na wiadomosc i odsyla jej poczatek 
 * Serwer pracujący w trybie wierszowym.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include "sockwrap.h"

#define MIN(a,b) (a)<(b)?(a):(b)
#define MAXLINE 100

// Ponizsze funkcje nadaja sie tylko do obslugi jednego gniazda naraz
// W szczegolnosci bufor trzyma dane pochodzace z jednego gniazda

char buffer[4096];
int buffer_len;		// ile jest wszystkich danych w buforze
int buffer_cnt;		// ile jest przeczytanych danych w buforze

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
	
int main()
{
	int sockfd = Socket(AF_INET, SOCK_STREAM, 0);   
	struct sockaddr_in server_address;
	bzero (&server_address, sizeof(server_address));
	server_address.sin_family      = AF_INET;
	server_address.sin_port        = htons(12345);
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);
	Bind (sockfd, &server_address, sizeof(server_address));
	Listen (sockfd, 64);        

	while (1) {	
		// accept() jak poprzednio, ale wypisujemy informacje na temat klienta
		struct sockaddr_in client_address;
		socklen_t len = sizeof(client_address);
		int conn_sockfd = Accept (sockfd, &client_address, &len);
		char ip_address[20];
		inet_ntop (AF_INET, &client_address.sin_addr, ip_address, sizeof(ip_address));
		printf ("New client %s:%d\n", ip_address, ntohs(client_address.sin_port));

		InitBuffer();
		int maxsize = 20;
		char recv_buffer[maxsize+1];
		int n;

		// Czekamy max. 7 sekund na kolejny wiersz i zapisujemy pierwsze
		// maxsize bajtow z tego wiersza do bufora recv_buffer.	
		while ( (n = ReadLine (conn_sockfd, recv_buffer, maxsize, 7)) ) {
			if (n < 0) {				
				printf ("DEBUG: Readline error: %s\n", n == -1 ? strerror(errno) : "timeout");
				break;	
			}
			recv_buffer[n] = 0;
			printf ("Chunk ->%s<- received\n", recv_buffer);

			// Odsylamy to co zapisalismy do bufora do klienta.  Uwaga: powinnismy
			// to robic podobnie jak w programie klienta, tj. wysylac dane do
			// skutku, a nie wywolywac pojedyncza funkcje send()
			Send (conn_sockfd, recv_buffer, n, 0);
		}
		
		Close (conn_sockfd);
		printf ("Disconnected\n");
	}
}

