#include <iostream>
#include <bitset>

using namespace std;

int main(int argc, char const* argv[])
{
  bitset<16> input = 0x00ff;
  bitset<16> mask  = 0xffff;

  cout << input.to_string() << endl;

  input ^= mask;

  input[15].flip();

  auto out = input.to_string();
  cout << out;

  return 0;
}
