#include <cstdio>
#include <cstdlib>

#define DEBUG 0

using namespace std;

// jakies long long inty wszedzie

int main() {
    int k, l;
    scanf("%d %d", &k, &l);

    int reps[l+1];

    reps[l] = 0;
    for (int i = 0; i < l; i++) {
        scanf("%d", &reps[i]);
    }

    long long int a[l+1];
    long long int b[l+1];

    a[l] = 0;
    for (int i=l-1; i>=0;i--) {
        a[i] = a[i+1] + reps[i];
    }

    b[l] = 0;
    for (int i=l-1; i>=0;i--) {
        b[i] = a[i] + b[i+1];
    }

    long long int t[l+1][k];
    int cuts[l+1][k];

    t[l][0] = 0;
    for (int i=l-1; i>=0;i--) {
        t[i][0] = b[i];
    }

    if (DEBUG) { printf("  i: "); for (int i=0;i<=l;i++) { printf("%d \t", i); }; printf("\n");    }
    if (DEBUG) { printf("a[]: "); for (int i=0;i<=l;i++) { printf("%d \t", a[i]); }; printf("\n"); }
    if (DEBUG) { printf("b[]: "); for (int i=0;i<=l;i++) { printf("%d \t", b[i]); }; printf("\n"); }

    // zmienic kolejnosc elementow tablicy
    for (int i=0;i<l;i++) {
        cuts[i][0] = l;
    }

    int vmin, imin, vcur, lastcut;
    for (int j=1;j<k;j++) {
        for (int i=0;i<l;i++) {
            vmin = t[i+1][j-1] + reps[i];
            imin = i+1;
            for (int s = i+2; s <= cuts[i][j-1]; s++) { // + s(i,s-1)
                vcur = t[s][j-1] + b[i] - b[s] - (s-i)*a[s];
                if (DEBUG == 2) { printf("i = %d | j = %d | s = %d | min = %d \t | val = %d\n", i, j, s, vmin, vcur); }
                if (vcur < vmin) {
                    vmin = vcur;
                    imin = s;
                }
            }
            t[i][j] = vmin;
            cuts[i][j] = imin;
        }
        t[l][j] = 0;

    }

    printf("%Ld\n", t[0][k-1]);

    if (DEBUG) { printf("Ciecia: "); int n = 0; for (int i=k-1;i>0;i--) { n = cuts[n][i]; printf("%d ",n); }; printf("\n"); }

    if (DEBUG) { for (int i=0;i<k;i++) { for (int j=0;j<=l;j++) { printf("%lld \t",t[j][i]); }; printf("\n"); } }

    if (DEBUG) { for (int i=0;i<k;i++) { for (int j=0;j<=l;j++) { printf("%lld \t",cuts[j][i]); }; printf("\n"); } }

    if (DEBUG) {
        for (int i=0;i<l;i++) {
            bool x = true;
            for (int j=0;j<k;j++) {
                    if (cuts[j][i] > cuts[j+1][i])
                        x = false;
            }
            if (!x)
                printf("Kolumna %d nie jest fajna.",i);
        }
    }

    int n = 0;
    int pre = 0;
    for (int i=k-1;i>0;i--) {
        pre = n;
        n = cuts[n][i];
        printf("%d ", n - pre);
    }
    printf("%d", l-n);


    return 0;
}
