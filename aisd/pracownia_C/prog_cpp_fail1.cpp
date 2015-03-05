#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include <vector>

#define DEBUG 0

using namespace std;

void print_status(vector<int> klaw[], int k) {
    if (DEBUG) {
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < klaw[i].size(); j++) {
                printf("%d ", klaw[i][j]);
            }
            printf("| ");
        }
        printf("\n");
    }
}

int main() {
    int k, l;
    scanf("%d %d", &k, &l);

    vector<int> klaw[k];
    int reps[l];

    for (int i = 0; i < l; i++) {
        scanf("%d", &reps[i]);
        //klaw[0].push_back(i);
    }



    // to można zrobić lepiej i ładniej i optymalniej
    {
        int sr = l / k;
        int i = 0;
        for (int j = 0; j < k; j++) {
            for (int z = 0; z < sr; z++) {
                klaw[j].push_back(i);
                i++;
            }
        }
        while (i < l) {
            klaw[k-1].push_back(i);
            i++;
        }
    }

    print_status(klaw, k);
    // for (int i=0;i<l;i++) printf("%d ", reps[i])



    bool iterujemy = true;
    int iteracja = 1;
    int current_cost, future_cost, lit, sz, e;
    while (iterujemy) {
        if (DEBUG) printf("== Iteracja %d\n", iteracja);
        iterujemy = false;

        // w prawo...
        // lit = 0;
        for (int i = 0; i < k-1; i++) {
            // sz = klaw[i].size();
            for (int p = 1; p <= klaw[i].size(); p++) {

                current_cost = 0;
                for (int i2 = 0; i2 < p; i2++) {
                    current_cost += (klaw[i].size() - i2) * reps[klaw[i][klaw[i].size() - i2 - 1]];
                }
                for (int i2 = 0; i2 < klaw[i+1].size(); i2++) {
                    current_cost += (i2 + 1) * reps[klaw[i+1][i2]];
                }

                future_cost = 0;
                for (int i2 = 0; i2 < p; i2++) {
                    future_cost += (p - i2) * reps[klaw[i][klaw[i].size() - i2 - 1]];
                }
                for (int i2 = 0; i2 < klaw[i+1].size(); i2++) {
                    future_cost += (i2 + 1 + p) * reps[klaw[i+1][i2]];
                }

                if (DEBUG) printf("[%d;%d] -> | Oplacalnosc: %d / %d \n", i, p, current_cost, future_cost);

                if (future_cost <= current_cost) {
                    for (int i2 = 0; i2 < p; i2++) {
                        e = klaw[i].back();
                        klaw[i].pop_back();
                        klaw[i+1].insert(klaw[i+1].begin(), e);
                    }
                    iterujemy = true;
                    print_status(klaw, k);
                    break;
                }

            }



            // lit += sz;
        }

        // i w lewo!
        // lit = l;
        for (int i = k-1; i > 0; i--) {
            // sz = klaw[i].size();

            for (int p = 1; p <= klaw[i].size(); p++) {
                current_cost = 0;
                for (int i2 = 0; i2 < klaw[i].size(); i2++) {
                    current_cost += (i2+1) * reps[klaw[i][i2]];
                    // current_cost += (klaw[i].size() - i2) * reps[lit - i2 - 1];
                }

                future_cost = 0;
                for (int i2 = 0; i2 < p; i2++) {
                    future_cost += (klaw[i-1].size() + i2 + 1) * reps[klaw[i][i2]];
                }
                for (int i2 = p; i2 < klaw[i].size(); i2++) {
                    future_cost += (i2 - p + 1) * reps[klaw[i][i2]];
                }
                //future_cost = (klaw[i-1].size() + 1) * reps[klaw[i][0]];
                //for (int i2 = 1; i2 < klaw[i].size(); i2++) {
                  //  future_cost += i2 * reps[klaw[i][i2]];
                    // przerobic
                    // future_cost += (klaw[i].size() - i2) * reps[lit - i2 - 1];
                //}


                if (DEBUG) printf("[%d;%d] <- | Oplacalnosc: %d / %d \n", i, p, current_cost, future_cost);

                if (future_cost < current_cost) {
                    for (int i2 = 0; i2 < p; i2++) {
                        e = klaw[i].front();
                        klaw[i].erase(klaw[i].begin());
                        klaw[i-1].push_back(e);
                    }
                    iterujemy = true;
                    print_status(klaw, k);
                    break;
                }
            }
            // lit += sz;
        }

        iteracja++;
    }

    print_status(klaw, k);

    int sum = 0;
    for (int i=0; i<k; i++) {
        for (int j=0; j<klaw[i].size(); j++) {
            sum += (j+1) * reps[klaw[i][j]];
        }
    }

    printf("%d\n", sum);
    for (int i=0; i<k; i++) {
        printf("%d ", klaw[i].size());
    }


    return 0;
}
