#include <cstdio>
#include <cstdlib>
#include <vector>
#include <tuple>
#include <set>


using namespace std;

#define MAXN 1000
#define HLIMIT 1000000001
#define DEBUG true
#define DEBUG2 false

int ufs[1000000];
int ufs_counter[1000000];

int uf_find(int a) {
	if (ufs[a] == a) {
		return a;
	} else {
		int fa = uf_find(ufs[a]);
		ufs[a] = fa;
		return fa;
	}
}

// returns removed number
int uf_union(int a, int b) {
	int fa = uf_find(a);
	int fb = uf_find(b);

	if (fa==fb) return -1;
	if (ufs_counter[fa] <= ufs_counter[fb]) {
		ufs_counter[fb] += ufs_counter[fa];
		ufs[fa] = fb;
		return fa;
	} else {
		ufs_counter[fa] += ufs_counter[fb];
		ufs[fb] = fa;
		return fb;
	}
}

int uf_create(int a) {
	ufs[a] = a;
	ufs_counter[a] = 1;
}

int find_best_fit(int request, int n, int data[]) {
        int half;
        int k = 0;
        int l = n;
        while (true) {
            half = (l + k) / 2;
            if (half == 0 || (data[half-1] < request && request <= data[half])) {
		return half;
            } else if (request <= data[half-1]) {
                l = half;
            } else if (request > data[half]) {
                k = half + ((l+k) % 2);
            } else {
		   printf("wat?\n");
	    } 
        }
}

int main() {
	int n, m;	
	scanf("%d %d", &n, &m);
if (DEBUG) { printf("Wczytano %d i %d\n", n, m);	 }
	int h[n][m];
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			scanf("%d", &h[i][j]);
			ufs[i*MAXN+j] = -1;
			ufs_counter[i*MAXN+j] = 0;
		}
	}

	int t;
	scanf("%d", &t);
if (DEBUG) {	printf("Wczytano %d\n", t); }
	int queries[t+1];
	int results[t+1];
	vector<pair<int,int>> qlists[t+1];
	for (int i = 0; i < t; i++) {
		if (DEBUG2) { printf("DUPA X\n"); }
		scanf("%d", &queries[i]);
		if (DEBUG2) { printf("DUPA Y\n"); }
	}
	queries[t] = HLIMIT;
	printf("Read whole input.\n");	
	// oprocz tablicy T z wartosciami do odpytywania, do każdej miejmy listę par (x,y) nowych pól, które w tej iteracji dochodzą
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			if (DEBUG) { printf("Thinking about (%d, %d)\n", i, j); }
			qlists[find_best_fit(h[i][j], t, queries)].push_back( pair<int,int>(j,i) );
		}
	}
	if (DEBUG) { printf("=== ZAPYTANIA: "); for(int i=0; i<=t; i++) { printf("%d ", queries[i]); } printf("\n"); }
	if (DEBUG) { printf("=== TABLICA h[][]\n"); for(int i = 0; i < n; i++) { for(int j = 0; j < m; j++) { printf("%d ", h[i][j]); }; printf("\n"); } }
	if (DEBUG) { printf("=== LISTY\n"); for(int i=0; i <= t; i++) { for(vector<pair<int,int>>::iterator it = qlists[i].begin(); it != qlists[i].end(); ++it) { printf("(%d,%d) ", get<0>(*it), get<1>(*it)); }; printf("\n"); } }

	return 0;
}
