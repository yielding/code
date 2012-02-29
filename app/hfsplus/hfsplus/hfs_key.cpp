#include "hfs_key.h"
#include <cstring>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HFSKey::HFSKey(uint32_t sz): m_size(sz)
{
  memset(m_fk, 0x00, sz);
}

HFSKey::HFSKey(HFSKey const& rhs)
{
  if (this != &rhs)
  {
    m_size   = rhs.m_size;
    m_aeskey = rhs.m_aeskey;
    memcpy(m_fk, rhs.m_fk, m_size);
  }
}

bool HFSKey::operator<(HFSKey const& rhs) const
{
  return memcmp(m_fk, rhs.m_fk, 32) < 0;
}

void HFSKey::set_decrypt(uint8_t* fk_, uint32_t sz)
{ 
  m_size = sz;
  memcpy(m_fk, fk_, m_size); 
  AES_set_decrypt_key(m_fk, m_size*8, &m_aeskey);
}

void HFSKey::set_encrypt(uint8_t* fk_, uint32_t sz)
{ 
  m_size = sz;
  memcpy(m_fk, fk_, m_size); 
  AES_set_encrypt_key(m_fk, m_size*8, &m_aeskey);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
