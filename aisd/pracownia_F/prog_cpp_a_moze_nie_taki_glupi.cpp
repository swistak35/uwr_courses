#include <cstdio>
#include <cstdlib>
#include <vector>
#include <tuple>
#include <set>


using namespace std;

int main() {
	int n, m;	
	scanf("%d %d", &n, &m);
	
	int h[n][m];
	char s[n][m];
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++) {
			scanf("%d", &h[i][j]);
			s[i][j] = 1;
		}
	}

	int t, req;
	int prev = 0;
	scanf("%d", &t);
	//int queries[t];
	for (int i = 0; i < t; i++) {
		scanf("%d", &req);
		
	}


	return 0;
}
