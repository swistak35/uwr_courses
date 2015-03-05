#include <cstdio>
#include <cstdlib>

using namespace std;

long long int max(long long int x, long long int y) { return (x > y ? x : y); }
long long int min(long long int x, long long int y) { return (x < y ? x : y); }

int main() {
    int n;
    long long int sum = 0, half;
    scanf("%d", &n);

    long long int tab[n];
    for (int i = 0; i < n; i++) {
        scanf("%lld", &tab[i]);
        sum += tab[i];
    }

    half = sum / 2;

    int a = 0, b = 0;
    long long int dist = 0;
    long long int mx = dist;

    while (b < n) {
        if (dist <= half) {
            dist += tab[b];
            b++;
            mx = max(mx, min(dist, sum-dist));
        }

        if (dist > half) {
            dist -= tab[a];
            a++;
            mx = max(mx, min(dist, sum-dist));
        }
    }

    printf("%lld\n", mx);

    return 0;
}
