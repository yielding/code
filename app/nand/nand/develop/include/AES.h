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
    AES(ByteBuffer const& kek);
    AES(ByteBuffer const& key, int mode, ByteBuffer const& iv);
   ~AES();

    auto encrypt(ByteBuffer b) -> ByteBuffer;
    auto decrypt(ByteBuffer b) -> ByteBuffer;

    auto unwrap(ByteBuffer& wrapped) -> ByteBuffer;
    auto   wrap(ByteBuffer& data)    -> ByteBuffer;

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
