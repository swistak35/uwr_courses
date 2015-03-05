#include <cstdio>

using namespace std;

int main() {
    int n,m,request;
    scanf("%d", &n);

    int data[n+1];
    for (int i=0;i<n;i++) {
        scanf("%d", &data[i]);
    }
    data[n] = 2000000001;

    scanf("%d", &m);
    for (int i=0;i<m;i++) {
        scanf("%d", &request);

        bool found = false;
        int x;
        int k = 0;
        int l = n;
        while (!found) {
            x = (l + k) / 2;
            if (request <= data[x-1]) {
                l = x;
            } else if (request > data[x]) {
                k = x + ((l+k) % 2);
            } else if (x == 0 || (data[x-1] < request && request <= data[x])) {
                printf("%d ", n-x);
                found = true;
            }
        }
    }

    return 0;
}
