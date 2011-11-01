#include <fstream>
#include <stdint.h>

using namespace std;
int main (int argc, char const* argv[])
{
  ofstream out;
  out.open("log.bin", ios_base::binary);

  for (uint16_t i=0; i<256; ++i) 
    out.write((char *)&i, 2);

  return 0;
}
