#include <cstdio>

using namespace std;

int main() {
    int n,m;
    scanf("%d %d", &n, &m);

    vector<pair<int,int>> edges[n];

    for (int i=0;i<m;i++) {
        int n1,n2,c;
        scanf("%d %d %d", &n1, &n2, &c);
        n1--;
        n2--;
        edges[n1].push_back(make_pair(n2, c));
        edges[n2].push_back(make_pair(n1, c));
    }



    return 0;
}
