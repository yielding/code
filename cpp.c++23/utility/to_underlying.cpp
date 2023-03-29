#include <utility>
#include <iostream>

using namespace std;

enum class Color : uint32_t 
{
  Red   = 0xff0000,
  Green = 0x00ff00,
  Blue  = 0x0000ff
};

Color r { Color::Red };

int main(int argc, char *argv[])
{
  auto value2 = to_underlying(r);
  cout << value2 << endl;
  
  return 0;
}