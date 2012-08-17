#include "AES.h"

#include <openssl/aes.h>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class AESImpl
{
public:
    AESImpl(ByteBuffer const& key, int mode, ByteBuffer const& iv);

    auto encrypt(ByteBuffer b) -> ByteBuffer;
    auto decrypt(ByteBuffer b) -> ByteBuffer;

private:
    int m_mode;
    ByteBuffer m_key;
    ByteBuffer m_iv;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
AESImpl::AESImpl(ByteBuffer const& key, int mode, ByteBuffer const& iv)
{
    m_mode = mode;
    m_key  = key;
    m_iv   = iv;
}

auto AESImpl::encrypt(ByteBuffer b) -> ByteBuffer
{
    ByteBuffer res(b.size());
    AES_KEY key;
    ::AES_set_encrypt_key((uint8_t*)m_key, int(m_key.size()) * 8, &key);

    if (m_mode == AES::CBC)
    {
        ::AES_cbc_encrypt(b, res, b.size(), &key, m_iv, AES_ENCRYPT);
    }
    else if (m_mode == AES::DEFAULT)
    {
        if (b.size() != 16)
            throw std::runtime_error("block size should be 16");

        ::AES_encrypt(b, res, &key);
    }
    else
    {
        throw std::runtime_error("not emplemented");
    }

    return res;
}

auto AESImpl::decrypt(ByteBuffer b) -> ByteBuffer
{
    ByteBuffer res(b.size());
    AES_KEY key;
    ::AES_set_decrypt_key((uint8_t*)m_key, int(m_key.size()) * 8, &key);

    if (m_mode == AES::CBC)
    {
        ::AES_cbc_encrypt(b, res, b.size(), &key, m_iv, AES_DECRYPT);
    }
    else if (m_mode == AES::DEFAULT)
    {
        if (b.size() != 16)
            throw std::runtime_error("block size should be 16");

        ::AES_decrypt(b, res, &key);
    }
    else
    {
        throw std::runtime_error("not emplemented");
    }

    return res;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
AES::AES(ByteBuffer const& key, int mode, ByteBuffer const& iv)
{
    pimpl = new AESImpl(key, mode, iv);
}

AES::~AES()
{
    delete pimpl;
}

auto AES::encrypt(ByteBuffer b) -> ByteBuffer
{
    return pimpl->encrypt(b);
}

auto AES::decrypt(ByteBuffer b) -> ByteBuffer
{
    return pimpl->decrypt(b);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
    ByteBuffer remove_padding(size_t block_size, ByteBuffer s)
    {
        auto pad_count = s.last();
        if (pad_count > block_size || pad_count > s.size())
            throw std::runtime_error("invalid padding");

        return s.slice(0, s.size() - pad_count);
    }
}

ByteBuffer
aes_decrypt_cbc(ByteBuffer data, ByteBuffer const& key, 
                ByteBuffer const& iv, bool padding)
{
    if (data.size() % 16)
        data = data.slice(0, (data.size() / 16) * 16); // trim the padding

    AES aes(key, AES::CBC, iv);
    data = aes.decrypt(data);

    return padding 
        ? remove_padding(16, data)
        : data;
}

ByteBuffer
aes_encrypt_cbc(ByteBuffer data, ByteBuffer const& key, 
                ByteBuffer const& iv, bool padding)
{
    if (data.size() % 16)
        data = data.slice(0, (data.size() / 16) * 16);

    AES aes(key, AES::CBC, iv);
    return aes.encrypt(data);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
