#include "emf_file.h"

#include <openssl/aes.h>
#include <openssl/sha.h>

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <string>

using namespace std;
using namespace boost;
namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EMFFile::EMFFile(HFSVolume* volume, HFSPlusForkData const& fork
    , HFSCatalogNodeID fileID , HFSKey const& filekey , bool deleted)
    : HFSFile(volume, fork, fileID, deleted)
{
    m_protection_version = volume->protection_version();
    m_decrypt_offset = 0;
    m_file_key = filekey;

    init_ivkey();
}

EMFFile::EMFFile(HFSVolume* volume , HFSKey const& filekey , uint16_t protection_version)
    : HFSFile(volume)
{
    m_protection_version = protection_version;
    m_decrypt_offset = 0;
    m_file_key = filekey;

    init_ivkey();
}

void EMFFile::init_ivkey()
{
    if (m_protection_version == 4)
    {
        // REMARK fk of filekey is updated..
        auto fk = m_file_key.as_buffer();

        SHA_CTX ctx;
        SHA1_Init(&ctx);
        SHA1_Update(&ctx, fk, 32);
        SHA1_Final(fk, &ctx);

        m_ivkey.set_encrypt(fk, 16);
    }
}

EMFFile::~EMFFile()
{
}

void EMFFile::decrypt_file()
{
    m_decrypt_offset = 0;
    auto bs = m_block_size;
    for (auto it = m_extents.begin(); it != m_extents.end(); ++it) 
    {
        for (uint32_t i=0; i<it->blockCount; ++i)
        {
            auto  lba = int64_t(it->startBlock) + i;
            auto from = lba * bs;
            ByteBuffer buffer = m_volume->read(from, bs);
            if (buffer.size() == bs)
            {
                auto b = process_block(lba, buffer, bs);
                // TODO
                m_volume->write(from, b);
            }
        }
    }
}

uint32_t EMFFile::start_lba()
{
    return m_extents[0].startBlock;
}

auto EMFFile::decrypt_buffer(uint32_t lba, ByteBuffer& buffer)
    -> ByteBuffer&
{
    return process_block(lba, buffer, buffer.size());
}

auto EMFFile::process_block(int64_t lba, ByteBuffer& buffer, uint32_t bs)
    -> ByteBuffer&
{
    auto v = dynamic_cast<EMFVolume*>(m_volume);

    uint32_t iv[4] = { 0 };
    uint8_t*     b = buffer;
    auto&   emfkey = v->emfkey().as_aeskey();

    // 1. re-encrypt using emf key
    v->iv_for_lba(uint32_t(lba), iv);
    AES_cbc_encrypt(b, b, bs, &emfkey, (uint8_t*)iv, AES_ENCRYPT);

    // 2. decrypt with filekey
    auto filekey = m_file_key.as_aeskey();
    if (v->protection_version() < 4)
    {
        v->iv_for_lba(uint32_t(lba), iv);
        AES_cbc_encrypt(buffer, buffer, bs, &filekey, (uint8_t*)iv, AES_DECRYPT);
    }
    else if (v->protection_version() == 4)
    {
        uint32_t iv_out[4] = { 0 };
        auto  size = (bs == m_block_size) ? 0x1000 : bs;
        auto& ivkey = m_ivkey.as_aeskey();

        for (uint32_t i=0; ; i++)
        {
            v->iv_for_lba((uint32_t)m_decrypt_offset, iv, false);
            AES_encrypt((const uint8_t*)iv, (uint8_t*)iv_out, &ivkey);
            AES_cbc_encrypt(b + i*size, b + i*size, size, &filekey, 
                    (uint8_t*)iv_out, AES_DECRYPT);
            m_decrypt_offset += size;
            if (bs != m_block_size || i >= bs/0x1000 - 1)
                break;
        }
    }
    else
    {
        throw std::runtime_error("EMFFile::process_block error");
    }

    return buffer;
}

bool EMFFile::decrypted_correctly(ByteBuffer& b)
{
    char const* magics[] = { 
        "SQLite", "bplist", "<?xml", "\xFF\xD8\xFF", "\xCE\xFA\xED\xFE",
        "GIF87a", "GIF89a", "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"
    };

    if (b.size() < 3)
        return false;

    int size = sizeof(magics)/sizeof(magics[0]);
    for (int i=0; i<size; i++)
    {
        string m(magics[i]);
        size_t size = size_t(b.size() < 10 ? b.size() : 10);
        string ss((char const*)b, size);

        if (boost::starts_with(ss, m))
            return true;
    }

    // mpeg4 test
    if (b.size() < 8)
        return false;

    uint8_t mpeg4[] = { 0x66, 0x74, 0x79, 0x70 };
    auto size2 = sizeof(mpeg4) / sizeof(mpeg4[0]);
    auto buffer = ((uint8_t*)b) + 4;
    bool found  = false;
    for (int i=0; i<(int)size2; i++)
    {
        if (buffer[i] != mpeg4[i])
        {
            found = true;
            break;
        }
    }

    return !found;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
