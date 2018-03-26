#include <iostream>

using namespace std;

int main(int argc, const char *argv[])
{
  alignas(float) unsigned char c[sizeof(float)];

  cout << sizeof(c);

  return 0;
}
