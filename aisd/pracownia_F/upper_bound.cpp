#include <cstdio>
#include <set>

using namespace std;
int main ()
{
  std::set<int> myset;
    std::set<int>::iterator itlow,itup;

      for (int i=1; i<10; i++) myset.insert(i*10); // 10 20 30 40 50 60 70 80 90

        itlow=myset.lower_bound(35);                //       ^
          itup=myset.upper_bound(35);                 //                   ^
printf("Lower bound: %d\n", *itlow);
printf("Upper bound: %d\n", *itup);

        itlow=myset.lower_bound(30);                //       ^
          itup=myset.upper_bound(30);                 //                   ^
printf("Lower bound: %d\n", *itlow);
printf("Upper bound: %d\n", *itup);
            // myset.erase(itlow,itup);                     // 10 20 70 80 90

                for (std::set<int>::iterator it=myset.begin(); it!=myset.end(); ++it)
			printf("%d ", *it);
                        return 0;
                        }
