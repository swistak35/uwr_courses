#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <netinet/ip_icmp.h>
#include <assert.h>
#include "sockwrap.h"
#include "in_cksum.h"

#define ICMP_HEADER_LEN 8

#define MAX_TTL 4
#define ICMP_PACKETS 1

unsigned char buffer[IP_MAXPACKET+1];
unsigned char * buffer_ptr;
int remaining_packet_data;

void print_bytes(int count) {
    for (int i=0; i<count; i++) {
        printf ("%.2x ", *buffer_ptr);
        buffer_ptr++; remaining_packet_data--;
    }
    printf("\n");
}


void prepare_initial_icmp_packet(struct icmp * icmp_packet, int id) {
    icmp_packet->icmp_type  = ICMP_ECHO;
    icmp_packet->icmp_code  = 0;
    icmp_packet->icmp_id    = id;
    icmp_packet->icmp_seq   = 1;
    icmp_packet->icmp_cksum = 0;
    icmp_packet->icmp_cksum = in_cksum((u_short*)icmp_packet, 8, 0);
}

void prepare_next_icmp_packet(struct icmp * icmp_packet) {
    icmp_packet->icmp_seq++;
    icmp_packet->icmp_cksum = 0;
    icmp_packet->icmp_cksum = in_cksum((u_short*)icmp_packet, 8, 0);
}

int main(int argc, char ** argv) {
    (void) argc;

    int sockfd = Socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);

    // Adres do ktorego bedziemy wysylac komunikat ICMP (google.pl)
    struct sockaddr_in remote_address;
    bzero (&remote_address, sizeof(remote_address));
    remote_address.sin_family   = AF_INET;
    inet_pton(AF_INET, argv[1], &remote_address.sin_addr);

    struct icmp icmp_packet;
    prepare_initial_icmp_packet(&icmp_packet, 123);

    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    char str[20];
    struct ip* recv_packet;
    struct icmp* recv_icmp_packet;

    for (int ttl = 1; ttl <= MAX_TTL; ttl++) {
        for (int trial = 0; trial < ICMP_PACKETS; trial++) {
            // Ustawiamy pole TTL pakietu na zadane
            Setsockopt (sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));

            // Wysylamy tylko naglowek, bez dodatkowych danych
            Sendto(sockfd, &icmp_packet, ICMP_HEADER_LEN, 0, &remote_address, sizeof(remote_address));

            prepare_next_icmp_packet(&icmp_packet);
        }

        buffer_ptr = buffer;
        remaining_packet_data = Recvfrom(sockfd, buffer_ptr, IP_MAXPACKET, 0, &sender, &sender_len);

        inet_ntop(AF_INET, &(sender.sin_addr), str, sizeof(str));
        printf ("\nReceived IP packet with ICMP content from: %s\n", str);

        // Budowe naglowka IP i ICMP (w szczegolnosci opis struktury) mozna znalezc
        // w plikach naglowkowych /usr/include/netinet/ip.h i ip_icmp.h

        // Na poczatku bufora jest naglowek IP
        recv_packet = (struct ip*) buffer_ptr;
        printf("IP header: ");
        print_bytes(recv_packet->ip_hl * 4);

        // Nastepnie na pewno jest ICMP enkapsulowany w pakiecie IP
        recv_icmp_packet = (struct icmp*) buffer_ptr;
        printf("ICMP type = %d, code = %d: ", recv_icmp_packet->icmp_type, recv_icmp_packet->icmp_code);
        print_bytes(ICMP_HEADER_LEN);

        // Pierwszy interesujacy typ pakietu to odpowiedzi TIME_EXCEEDED
        if (recv_icmp_packet->icmp_type == ICMP_TIME_EXCEEDED &&
                recv_icmp_packet->icmp_code == ICMP_EXC_TTL) {

            // Pakiet ICMP generowany w momencie zmniejszenia TTL do zera,
            // zawiera w danych kopie pakietu IP, ktoremu pole TTL spadlo do zera

            struct ip* packet_orig = (struct ip*) buffer_ptr;
            printf("Original IP header: ");
            print_bytes(packet_orig->ip_hl * 4);

            if (packet_orig->ip_p == IPPROTO_ICMP) {

                // Ten pakiet zostal wygenerowany w odpowiedzi na pakiet IP:ICMP
                // (byc moze na nasz -- trzeba sprawdzic w tym celu pola id i seq)
                printf("Contains ICMP packet: ");
                print_bytes(ICMP_HEADER_LEN);
                assert(remaining_packet_data == 0);

            } else {

                // Ta czesc jest zbedna; tylko do debugowania
                printf("Original IP payload: ");
                print_bytes(remaining_packet_data);

            }
        }

        // Drugi interesujacy typ pakietow to odpowiedzi na echo
        if (recv_icmp_packet->icmp_type == ICMP_ECHOREPLY) {

            // Odbiorca odsyla nam komunikat ECHO REPLY (prawdopodobnie
            // w odpowiedzi na nasze ECHO REQUEST). Zeby to sprawdzic, nalezy
            // obejrzec pola id i seq pakietu ICMP.

            // Jesli to jest odpowiedz na nasze ECHO REQUEST, to payload
            // powinien zawierac same zera, bo my nie wysylalismy payloadu w ogole.
            printf("ICMP payload: ");
            print_bytes(remaining_packet_data);
        }
    }
    return 0;
}
