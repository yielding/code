#ifndef AES_H
#define AES_H

#include "ByteBuffer.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using utility::hex::ByteBuffer;

class AESImpl;

class AES
{
public:
    enum { DEFAULT = -1, CBC, ECB };

public:
    AES(ByteBuffer const& key);
    AES(ByteBuffer const& key, int mode, ByteBuffer const& iv);
   ~AES();

    auto encrypt(ByteBuffer const& b) -> ByteBuffer;
    auto decrypt(ByteBuffer const& b) -> ByteBuffer;

    auto unwrap(ByteBuffer const& wrapped) -> ByteBuffer;
    auto   wrap(ByteBuffer const& data)    -> ByteBuffer;

private:
    AESImpl* pimpl;
};

ByteBuffer
aes_decrypt_cbc(ByteBuffer data, ByteBuffer const& key, 
                ByteBuffer const& iv, bool padding=false);

ByteBuffer
aes_encrypt_cbc(ByteBuffer data, ByteBuffer const& key, 
                ByteBuffer const& iv, bool padding=false);

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
