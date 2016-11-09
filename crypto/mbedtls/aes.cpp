#include "mbedtls/aes.h"

#include <cstdint>

int main(int argc, char *argv[])
{
  uint8_t key[32];

  mbedtls_aes_context aes;
  ::mbedtls_aes_setkey_enc(&aes, key, 256);

  
  return 0;
}
