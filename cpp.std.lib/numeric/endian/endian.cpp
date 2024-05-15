#include <iostream>
#include <bit>
#include <iomanip>

using namespace std;

int main(int argc, char *argv[])
{
  cout << boolalpha << (endian::native == endian::little) << endl;

  if constexpr (endian::native == endian::big)
    cout << "big-endian" << "\n";
  else 
    cout << "little-endian" << "\n";
  
  return 0;
}

