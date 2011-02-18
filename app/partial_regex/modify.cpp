#include <stdint.h>
#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  fstream f;
  char buf[16];

  f.open("test_16g.bin", ios_base::in | ios_base::out | ios_base::binary);
  if (!f.is_open())
  {
    cout << "error\n";
    exit(EXIT_FAILURE);
  }

  int64_t pos = 1024LL*1024*1024*16 - 16;

  f.seekg(pos);
  f.read(buf, 16);

  buf[14] = 0xff;
  buf[15] = 0xff;
  for (int i=0; i<16; i++)
  {
    char b[5];
    sprintf(b, "%d ", (unsigned)buf[i]);
    cout << b;
  }

  cout << f.tellg() << endl;
  f.seekg(pos);
  cout << f.tellg() << endl;
  f.write(buf, 16);
  f.close();

  return 0;
}
