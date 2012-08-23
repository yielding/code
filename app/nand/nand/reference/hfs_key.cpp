#include "stdafx.h"
#include "hfs_key.h"

#include <cstring>

#include "ByteBuffer.h"

using namespace std;
using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HFSKey::HFSKey(uint32_t sz): m_size(sz)
{
    memset(m_fk, 0x00, sz);
}

HFSKey::HFSKey(string const& s, uint32_t sz, int dir)
{
    auto bytes = ByteBuffer::from_hexcode(s);

    dir == kDecrypt 
        ? this->set_decrypt((uint8_t*)bytes, sz)
        : this->set_encrypt((uint8_t*)bytes, sz);
}

HFSKey::HFSKey(uint8_t* bytes, uint32_t sz, int dir)
{
    dir == kDecrypt 
        ? this->set_decrypt((uint8_t*)bytes, sz)
        : this->set_encrypt((uint8_t*)bytes, sz);
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

auto HFSKey::copy() const -> HFSKey
{
    HFSKey cp(*this);
    return cp;
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

////////////////////////////////////////////////////////////////////////////////
//
// m_fk     : uint8_t buffer
// m_aeskey : AES_KEY equivalent to m_fk
//
////////////////////////////////////////////////////////////////////////////////
void HFSKey::set_encrypt(uint8_t* fk_, uint32_t sz)
{ 
    m_size = sz;
    memcpy(m_fk, fk_, m_size);              
    AES_set_encrypt_key(m_fk, m_size*8, &m_aeskey);
}

auto HFSKey::to_str() const -> string
{
    vector<uint8_t> v(m_fk, m_fk + m_size);

    return ByteBuffer::to_hexcode(v);	
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
