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

#define DEBUG 1
#define MAX_TTL 30
#define ICMP_PACKETS 3

#define dprintf(...) \
    do { if (DEBUG) printf(__VA_ARGS__); } while (0)

unsigned char buffer[IP_MAXPACKET+1];
unsigned char * buffer_ptr;
int remaining_packet_data;

// Wartosc na jaka beda ustawiane icmp_id
int program_id;

void print_status_line(int ttl, char ip[][20], int received_packet_count, int response_time_sum) {
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

void print_bytes(int count) {
    for (int i = 0; i < count; i++) {
        dprintf("%.2x ", *buffer_ptr + i);
    }
    dprintf("\n");
}

void decapsulate_data(int count) {
    buffer_ptr += count;
    remaining_packet_data -= count;
}

void prepare_initial_icmp_packet(struct icmp * icmp_packet) {
    icmp_packet->icmp_type  = ICMP_ECHO;
    icmp_packet->icmp_code  = 0;
    icmp_packet->icmp_id    = program_id;
    icmp_packet->icmp_seq   = 1;
    icmp_packet->icmp_cksum = 0;
    icmp_packet->icmp_cksum = in_cksum((u_short*)icmp_packet, 8, 0);
}

void prepare_next_icmp_packet(struct icmp * icmp_packet) {
    icmp_packet->icmp_seq++;
    icmp_packet->icmp_cksum = 0;
    icmp_packet->icmp_cksum = in_cksum((u_short*)icmp_packet, 8, 0);
}

void add_ip_address_to_set(char ip[][20], char str[]) {
    for (int i = 0; i < ICMP_PACKETS; i++) {
        if (strcmp(ip[i], "") == 0) {
            strcpy(ip[i], str);
            break;
        } else if (strcmp(ip[i], str) == 0) {
            break;
        }
    }
}

void debug_print_icmp_header(struct icmp * packet) {
    dprintf("ICMP type = %d, code = %d, id = %d, seq = %d\n",
            packet->icmp_type,
            packet->icmp_code,
            packet->icmp_id,
            packet->icmp_seq);
    dprintf("ICMP header: ");
    print_bytes(ICMP_HEADER_LEN);
}

// w plikach naglowkowych /usr/include/netinet/ip.h i ip_icmp.h

int main(int argc, char ** argv) {
    if (argc != 2) {
        printf("Run: ./traceroute <IPv4_ADDRESS>\n");
        exit(1);
    }

    // Ustaw z jakim icmp_id beda wysylane pakiety
    program_id = getpid();

    // Ustawianie socketu
    int sockfd = Socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);

    // Adres do ktorego bedziemy wysylac komunikat ICMP
    struct sockaddr_in remote_address;
    bzero(&remote_address, sizeof(remote_address));

    // Uzywamy IPv4
    remote_address.sin_family = AF_INET;
    if (inet_pton(AF_INET, argv[1], &remote_address.sin_addr) != 1) {
        printf("The argument is not valid IPv4 address.\n");
        exit(1);
    }

    struct icmp icmp_packet;
    prepare_initial_icmp_packet(&icmp_packet);

    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    char str[20];
    char ip[ICMP_PACKETS][20];
    struct ip* recv_packet;
    struct icmp* recv_icmp_packet;

    dprintf("My PID: %d\n", getpid());

    bool target_found = false;
    for (int ttl = 1; ttl <= MAX_TTL && !target_found; ttl++) {
        // Wysylanie pakietow
        for (int trial = 0; trial < ICMP_PACKETS; trial++) {
            Setsockopt (sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));
            Sendto(sockfd, &icmp_packet, ICMP_HEADER_LEN, 0, &remote_address, sizeof(remote_address));
            prepare_next_icmp_packet(&icmp_packet);
        }

        struct timeval timeout;
        timeout.tv_sec = 1;
        timeout.tv_usec = 0;

        for (int i = 0; i < ICMP_PACKETS; i++) {
            strcpy(ip[0], "");
            strcpy(ip[1], "");
            strcpy(ip[2], "");
        }

        int received_packet_count = 0;
        int response_time_sum = 0;

        while (true) {
            // Select dziala na zbiorze FD
            fd_set descriptors;
            FD_ZERO(&descriptors);
            FD_SET(sockfd, &descriptors);

            int ready = Select(sockfd+1, &descriptors, NULL, NULL, &timeout);

            // Jesli mielismy timeouta to koniec tej rundy
            if (ready == 0) {
                break;
            }

            // Odbierz pakiet z socketa
            buffer_ptr = buffer;
            remaining_packet_data = Recvfrom(sockfd, buffer_ptr, IP_MAXPACKET, 0, &sender, &sender_len);

            if (inet_ntop(AF_INET, &(sender.sin_addr), str, sizeof(str)) == NULL) {
                ERROR("inet_ntop error")
            }

            dprintf("\nReceived IP packet with ICMP content from: %s\n", str);

            // Dekapsulacja pakietu IP
            recv_packet = (struct ip*) buffer_ptr;
            dprintf("IP header: ");
            print_bytes(recv_packet->ip_hl * 4);
            decapsulate_data(recv_packet->ip_hl * 4);

            // Dekapsulacja ICMP
            recv_icmp_packet = (struct icmp*) buffer_ptr;
            debug_print_icmp_header(recv_icmp_packet);
            decapsulate_data(ICMP_HEADER_LEN);

            // TTL zbity do 0
            if (recv_icmp_packet->icmp_type == ICMP_TIME_EXCEEDED &&
                    recv_icmp_packet->icmp_code == ICMP_EXC_TTL) {

                // Pobranie z payloadu pakietu IP, ktoremu TTL zjechal do 0
                struct ip* packet_orig = (struct ip*) buffer_ptr;
                dprintf("Original IP header: ");
                print_bytes(packet_orig->ip_hl * 4);
                decapsulate_data(packet_orig->ip_hl * 4);

                if (packet_orig->ip_p == IPPROTO_ICMP) {
                    struct icmp* orig_icmp = (struct icmp*) buffer_ptr;
                    debug_print_icmp_header(orig_icmp);
                    decapsulate_data(ICMP_HEADER_LEN);

                    if (orig_icmp->icmp_id == program_id) {
                        int orig_ttl = ((orig_icmp->icmp_seq - 1) / ICMP_PACKETS) + 1;
                        int orig_number = (orig_icmp->icmp_seq - 1) % ICMP_PACKETS;

                        dprintf("Recovered data about sent packet: ttl=%d number=%d\n", orig_ttl, orig_number);

                        if (orig_ttl < ttl) {
                            dprintf("It's old TTL packet!\n");
                        } else {
                            add_ip_address_to_set(ip, str);
                            received_packet_count++;
                            response_time_sum += timeout.tv_usec;
                        }
                    }

                    assert(remaining_packet_data == 0);
                }
            }

            // Dostalismy odpowiedz ECHO Reply
            if (recv_icmp_packet->icmp_type == ICMP_ECHOREPLY) {
                dprintf("ICMP payload: ");
                print_bytes(remaining_packet_data);
                decapsulate_data(remaining_packet_data);

                int recv_orig_ttl = ((recv_icmp_packet->icmp_seq - 1) / ICMP_PACKETS) + 1;
                int recv_orig_number = (recv_icmp_packet->icmp_seq - 1) % ICMP_PACKETS;
                dprintf("Recovered data about received packet: ttl=%d number=%d\n", recv_orig_ttl, recv_orig_number);

                if (recv_icmp_packet->icmp_id == program_id) {
                    if (ttl == recv_orig_ttl) {
                        target_found = true;
                        add_ip_address_to_set(ip, str);
                        received_packet_count++;
                        response_time_sum += timeout.tv_usec;
                    }
                }
            }
        }

        print_status_line(ttl, ip, received_packet_count, response_time_sum);
    }

    return 0;
}
