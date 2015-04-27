#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <netinet/ip_icmp.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>

int main() {
    char str[] = "DATA 179 1200\nbladsfdsafdsa";
    int int1,int2;
    sscanf(str, "DATA %d %d\n", &int1, &int2);
    printf("Int1: %d\n", int1);
    printf("Int2: %d\n", int2);
    printf("Binary data: %b\n", binary);
    return 0;
}
