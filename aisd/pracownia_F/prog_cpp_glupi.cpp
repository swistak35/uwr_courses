#include <cstdio>
#include <cstdlib>
#include <vector>
#include <tuple>
#include <set>


using namespace std;

#define MAXN 1000
#define HLIMIT 1000000001
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

int find_best_fit(int x, int t, int queries[]) {
	for (int i = 0; i <= t; i++) {
		if (queries[i] >= x) {
			return i;
		}
	}
}


int main() {
	int n, m;	
	scanf("%d %d", &n, &m);
	
	// zrobmy union-find na całą planszę
	// przejdźmy po całej planszy i zróbmy początkowe uniony
	int h[n][m];
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			scanf("%d", &h[i][j]);
			ufs[i*MAXN+j] = i*MAXN + j;
			ufs_counter[i*MAXN+j] = 1;
			if (j != 0 && h[i][j] == h[i][j-1]) {
				uf_union(i*MAXN + j, i*MAXN + j - 1);
			}
			if (i != 0 && h[i][j] == h[i-1][j]) {
				uf_union(i*MAXN + j, (i-1)*MAXN + j);
			}
		}
	}

	int t;
	scanf("%d", &t);
	int queries[t+1];
	vector<pair<int,int>> qlists[t+1];
	for (int i = 0; i < t; i++) {
		scanf("%d", &queries[t]);
	}
	queries[t] = HLIMIT;

	// oprocz tablicy T z wartosciami do odpytywania, do każdej miejmy listę par (x,y) nowych pól, które w tej iteracji dochodzą
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			qlists[find_best_fit(h[i][j], t, queries)].push_back( pair<int,int>(i,j) );
		}
	}
	
	int x,y,r;
	set<int> current;
	for (int i = 0; i < t; i++) {
        	for (vector<pair<int,int>>::iterator it = qlists[i].begin(); it != qlists[i].end(); ++it) {
			x = get<0>(*it);
			y = get<1>(*it);
			current.insert(uf_find(y*MAXN + x));
			if (x > 0 && h[y][x-1] <= t) {
				r = uf_union(y*MAXN + x, y*MAXN + x - 1);
				current.erase(r);
			}
			if (y > 0 && h[y-1][x] <= t) {
				r = uf_union(y*MAXN + x, (y-1)*MAXN + x);
				current.erase(r);
			}
			// zadziala w mintescie?
			if (x < m-1 && h[y][x+1] <= t) {
				r = uf_union(y*MAXN + x, y*MAXN + x + 1);
				current.erase(r);
			}
			if (y < n-1 && h[y+1][x] <= t) {
				r = uf_union(y*MAXN + x, (y+1)*MAXN + x);
				current.erase(r);
			}
		}
		printf("%d ", current.size());
	}
	// w i-tej iteracji, dla kazdego nowego pola sprawdzamy sąsiadów, jeśli są różne wartości, to robimy uniony (co najwyżej 4 czy coś takiego)
	// utrzymujemy listę wskaźników, ile jest różnych wysp?
	return 0;
}
