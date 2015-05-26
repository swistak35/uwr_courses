#include <iostream>
#include <cstdint>

using namespace std;

int main() {
  int x = 255;
  char c = -1; 
  cout << hex << x << endl;
  cout << hex << +c << endl;
  if (x == c) {
    cout << "Kurwa..." << endl;
  }
  cout << sizeof(int) <<endl;
  cout << sizeof(uint16_t) << endl;
  return 0;
}
