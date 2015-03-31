#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <netinet/ip_icmp.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include "sockwrap.h"
#include "in_cksum.h"

#define ICMP_HEADER_LEN 8

#define DEBUG 0
#define MAX_TTL 20
#define ICMP_PACKETS 3

#define dprintf(...) \
    do { if (DEBUG) printf(__VA_ARGS__); } while (0)

unsigned char buffer[IP_MAXPACKET+1];
unsigned char * buffer_ptr;
int remaining_packet_data;

void print_bytes(int count) {
    for (int i=0; i<count; i++) {
        dprintf("%.2x ", *buffer_ptr);
        buffer_ptr++; remaining_packet_data--;
    }
    dprintf("\n");
}


void prepare_initial_icmp_packet(struct icmp * icmp_packet, int id) {
    icmp_packet->icmp_type  = ICMP_ECHO;
    icmp_packet->icmp_code  = 0;
    icmp_packet->icmp_id    = getpid();
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
    char ip[ICMP_PACKETS][20];
    struct ip* recv_packet;
    struct icmp* recv_icmp_packet;

    printf("My PID: %d\n", getpid());

    bool target_found = false;

    for (int ttl = 1; ttl <= MAX_TTL && !target_found; ttl++) {
        for (int trial = 0; trial < ICMP_PACKETS; trial++) {
            // Ustawiamy pole TTL pakietu na zadane
            Setsockopt (sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));

            // Wysylamy tylko naglowek, bez dodatkowych danych
            Sendto(sockfd, &icmp_packet, ICMP_HEADER_LEN, 0, &remote_address, sizeof(remote_address));

            prepare_next_icmp_packet(&icmp_packet);
        }

        struct timeval timeout;
        timeout.tv_sec = 1;
        timeout.tv_usec = 0;

        strcpy(ip[0], "");
        strcpy(ip[1], "");
        strcpy(ip[2], "");
        int received_packet_count = 0;
        int response_time_sum = 0;

        while (1) {
            fd_set descriptors;

            FD_ZERO(&descriptors);
            FD_SET(sockfd, &descriptors);

            int ready = Select(sockfd+1, &descriptors, NULL, NULL, &timeout);
            dprintf("we waited %.2f seconds with result (%d)\n", 1 - (timeout.tv_sec + timeout.tv_usec * 1.0 / 1000000), ready);
            if (ready == 0) {
                break;
            }

            buffer_ptr = buffer;
            remaining_packet_data = Recvfrom(sockfd, buffer_ptr, IP_MAXPACKET, 0, &sender, &sender_len);

            inet_ntop(AF_INET, &(sender.sin_addr), str, sizeof(str));
            dprintf("\nReceived IP packet with ICMP content from: %s\n", str);

            // Budowe naglowka IP i ICMP (w szczegolnosci opis struktury) mozna znalezc
            // w plikach naglowkowych /usr/include/netinet/ip.h i ip_icmp.h

            // Na poczatku bufora jest naglowek IP
            recv_packet = (struct ip*) buffer_ptr;
            dprintf("IP header: ");
            print_bytes(recv_packet->ip_hl * 4);

            // Nastepnie na pewno jest ICMP enkapsulowany w pakiecie IP
            recv_icmp_packet = (struct icmp*) buffer_ptr;
            dprintf("ICMP type = %d, code = %d: ", recv_icmp_packet->icmp_type, recv_icmp_packet->icmp_code);
            print_bytes(ICMP_HEADER_LEN);

            // Pierwszy interesujacy typ pakietu to odpowiedzi TIME_EXCEEDED
            if (recv_icmp_packet->icmp_type == ICMP_TIME_EXCEEDED &&
                    recv_icmp_packet->icmp_code == ICMP_EXC_TTL) {

                // Pakiet ICMP generowany w momencie zmniejszenia TTL do zera,
                // zawiera w danych kopie pakietu IP, ktoremu pole TTL spadlo do zera

                struct ip* packet_orig = (struct ip*) buffer_ptr;
                dprintf("Original IP header: ");
                print_bytes(packet_orig->ip_hl * 4);

                if (packet_orig->ip_p == IPPROTO_ICMP) {

                    // Ten pakiet zostal wygenerowany w odpowiedzi na pakiet IP:ICMP
                    // (byc moze na nasz -- trzeba sprawdzic w tym celu pola id i seq)
                    struct icmp* orig_icmp = (struct icmp*) buffer_ptr;
                    dprintf("ICMP type = %d, code = %d: ", orig_icmp->icmp_type, orig_icmp->icmp_code);
                    dprintf("ID: %d SEQ: %d \n", orig_icmp->icmp_id, orig_icmp->icmp_seq);
                    dprintf("Contains ICMP packet: ");
                    print_bytes(ICMP_HEADER_LEN);

                    if (orig_icmp->icmp_id == getpid()) {
                        int orig_ttl = ((orig_icmp->icmp_seq - 1) / 3) + 1;
                        int orig_number = (orig_icmp->icmp_seq - 1) % 3;
                        dprintf("orig ttl=%d number=%d\n", orig_ttl, orig_number);
                        if (orig_ttl < ttl) {
                            dprintf("Old TTL packet!\n");
                        } else {
                            strcpy(ip[orig_number], str);
                            received_packet_count++;
                            response_time_sum += timeout.tv_usec;
                        }
                    }
                    assert(remaining_packet_data == 0);

                } else {
                    // Ta czesc jest zbedna; tylko do debugowania
                    dprintf("Original IP payload: ");
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
                dprintf("ICMP payload: ");
                dprintf("ID: %d SEQ: %d \n", recv_icmp_packet->icmp_id, recv_icmp_packet->icmp_seq);
                int recv_orig_ttl = ((recv_icmp_packet->icmp_seq - 1) / 3) + 1;
                int orig_number = (recv_icmp_packet->icmp_seq - 1) % 3;
                if (recv_icmp_packet->icmp_id == getpid() && ttl == recv_orig_ttl) {
                    target_found = true;
                    strcpy(ip[orig_number], str);
                    received_packet_count++;
                    response_time_sum += timeout.tv_usec;
                }
                print_bytes(remaining_packet_data);
            }
        }

        printf("%d. ", ttl);

        for (int i = 0; i < ICMP_PACKETS; i++) {
            if (strcmp(ip[i], "") != 0) {
                printf("%s ", ip[i]);
            }
        }

        if (received_packet_count == ICMP_PACKETS) {
            int avg_resp_time = response_time_sum / ICMP_PACKETS / 1000;
            printf("%dms", avg_resp_time);
        } else if (received_packet_count > 0) {
            printf("???");
        } else {
            printf("*");
        }

        printf("\n");
    }

    return 0;
}
