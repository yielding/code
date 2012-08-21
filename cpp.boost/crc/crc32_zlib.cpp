#include <zlib.h>

#include <iostream>
#include <stdint.h>

using namespace std;

int main(int argc, const char *argv[])
{
  char const* a = "1234567890";

  uint32_t res = crc32(0, (uint8_t*)a, 10);
  cout << res;
  
  return 0;
}
