#include <cstdio>

/*
	Rafał Łasocha
	258338
	KLo
*/

int main() {
	int a,b;

	scanf("%d %d", &a, &b);

	if (a % 2 == 0) {
		a++;
	}

	for (int i=a; i <= b; i += 2) {
		printf("%d ",i);
	}

	return 0;
}