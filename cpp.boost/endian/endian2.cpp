#include <boost/endian/buffers.hpp>

#include <fstream>
#include <iostream>
#include <string>
#include <cstdint>
#include <iomanip>

using namespace std;
using namespace boost::endian;

int main(int argc, char *argv[])
{
  uint32_t a = 0x01020304;

  auto filename = "data.txt";
  {
  ofstream ofs;
  ofs.open(filename, ios_base::binary);

  ofs.write(reinterpret_cast<char*>(&a), sizeof(uint32_t));
  }

  int b;
  {
  ifstream ifs;
  ifs.open(filename, ios_base::binary);
  ifs.read(reinterpret_cast<char*>(&b), 4);
  cout << hex << b << endl;
  b = ::endian_reverse((uint32_t)b);
  cout << hex << b << endl;
  }
  
  return 0;
}
