#include <iostream>
#include <bit>
#include <iomanip>

using namespace std;

int main(int argc, char *argv[])
{
  cout << boolalpha << (endian::native == endian::little) << endl;
  
  return 0;
}
