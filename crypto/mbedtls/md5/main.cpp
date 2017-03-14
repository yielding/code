#include "md5.h"

#include <cstdio>

using namespace std;

void print_hex(string const& digest)
{
  for (int i=0; i<16; ++i) printf("%02x", (uint8_t)digest[i]);
  printf("\n");
}

int main(int argc, char *argv[])
{
  auto msg = "Hello, world!"s;
  auto d0  = mbedtls::md5::hash(msg);
  print_hex(d0);

  mbedtls::md5 m;
  m.update(msg);
  auto d1 = m.finish();
  print_hex(d1);

  return 0;
}
