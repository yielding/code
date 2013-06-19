#include <iostream>

using namespace std;

enum class altitude: char
{
  high = 'h',
  low  = 'l'
};

int main(int argc, const char *argv[])
{
  altitude a = altitude::low;

  cout << static_cast<char>(a);

  return 0;
}
