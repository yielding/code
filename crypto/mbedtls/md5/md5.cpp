#include "mbedtls/md5.h"

#include <cstdio>
#include <string>

using namespace std;

int main(int argc, char *argv[])
{
  uint8_t digest[16];
  auto str = "Hello, world!"s;

  mbedtls_md5((uint8_t *)str.c_str(), 13, digest);

  for (int i=0; i<16; ++i) 
    printf("%02x", digest[i]);
  
  return 0;
}
