#include "cipher.h"

#include <iostream>

namespace OpenSSL {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
Cipher::Cipher(std::string const& name)
  : m_init_ok(false)
{
  EVP_CIPHER_CTX* ctx;

  std::cout << "getting the cipher of " << name << std::endl;
  EVP_CIPHER const* cipher = EVP_get_cipherbyname(name.c_str());
//  EVP_CIPHER const* cipher = EVP_aes_256_cbc();
  if (cipher != nullptr)
  {
    std::cout << "cipher is not null\n";
    if (EVP_CipherInit_ex(ctx, cipher, NULL, NULL, NULL, -1) == 1)
    {
      m_init_ok = true;
      std::cout << "init ok\n";
    }
  }
  else
  {
    std::cout << "cipher is null\n";
  }
}

Cipher::~Cipher()
{
}

bool Cipher::ok()
{
  return m_init_ok;
}

Cipher& Cipher::reset()
{
  return *this;
}

Cipher& Cipher::encrypt()
{
  return *this;
}

Cipher& Cipher::update()
{
  return *this;
}

void Cipher::final()
{
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
};
