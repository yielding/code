#include <stdint.h>
#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  if (argc != 2)
  {
    cout << "usage: filler xxx.bin\n";
    return 0;
  }

  fstream f;
  f.open(argv[1], ios_base::in | ios_base::out | ios_base::binary);
  if (!f.is_open())
  {
    cout << "error cannot open " << argv[1] << endl;
    exit(EXIT_FAILURE);
  }

  f.seekg(0, ios::end);
  int64_t size = f.tellg();
  f.seekg(0);

  uint8_t block[16] = { 
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 
    0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff };

  int64_t block_size = size / 16;
  for (int64_t i=0; i<block_size; ++i)
    f.write((char*)block, 16);

  f.seekg(0);
  uint8_t c = 0xff;
  uint8_t d = 0xff;
  f.write((char*)&c, 1);
  f.write((char*)&d, 1);

  f.seekg(size-2);
  f.write((char*)&c, 1);
  f.write((char*)&d, 1);

  f.close();
    
  return 0;
}
