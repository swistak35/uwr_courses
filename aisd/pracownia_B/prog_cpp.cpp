#include <cstdio>
#include <cstdlib>

#define RMOD 999979

using namespace std;

bool check_lt(char x) {
    return (x == '1' || x == '3' || x == '5' || x == '7');
}

bool check_ul(char x) {
    return (x == '2' || x == '3' || x == '6' || x == '7');
}

bool check_up(char x) {
    return (x == '4' || x == '5' || x == '6' || x == '7');
}

int main() {
    // count_pre i count_tab powinny miec pewnie cos wiekszego niz int, albo robic modulo odpowiednio wczesnie
    // nasz program nie dziala dla m = 0 lub n = 0
    int m, n;
    char str[100];
    fgets(str, 100, stdin);
    sscanf(str, "%d %d", &m, &n);

    char pre[n+3];
    char tab[n+3];

    int count_pre[n+1];
    int count_tab[n+1];

    fgets(pre, n+3, stdin);
    // printf("fgets: %s\n", pre);

    count_pre[0] = 1;
    for (int i = 1; i <= n; i++) {
        if (check_lt(pre[i-1])) {
            count_pre[i] = count_pre[i-1];
        } else {
            count_pre[i] = 0;
        }

    }


    for (int k=1; k <= m; k++) {
        fgets(tab, n+3, stdin);
        // printf("fgets: %s\n", tab);

        for (int i=0; i <= n; i++) {
            count_tab[i] = 0;

            if (i != 0) {
                if (check_lt(tab[i-1])) {
                    count_tab[i] += count_tab[i-1];
                }

                if (check_ul(pre[i-1])) {
                    count_tab[i] += count_pre[i-1];
                }
            }


            if (check_up(pre[i])) {
                count_tab[i] += count_pre[i];
            }
            count_tab[i] %= RMOD;
        }

        for (int i = 0; i <= n; i++) {
            count_pre[i] = count_tab[i];
            // printf("%d ", count_tab[i]);
        }
        // printf("\n");

        for (int i = 0; i <= n; i++) {
            pre[i] = tab[i];
        }
    }

    printf("%d", count_tab[n]);

    
    return 0;
}
