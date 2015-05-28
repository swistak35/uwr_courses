#include <iostream>
#include <cstdint>

using namespace std;

class SsijMiPale {

  public:
  void tescik() {
    static int chuj = 0;
    printf("chuj = %d\n", chuj);
    chuj++;
  }
};

int main() {
  SsijMiPale * serio = new SsijMiPale();
  serio->tescik();
  serio->tescik();
  SsijMiPale * bardzo_serio = new SsijMiPale();
  bardzo_serio->tescik();
  bardzo_serio->tescik();

  return 0;
}
