#include "emf_file.h"

#include <openssl/aes.h>
#include <openssl/sha.h>

#include <boost/format.hpp>
#include <boost/filesystem.hpp>
#include <string>

using namespace std;
using namespace boost;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  std::string get_new_filepath(char const* name, char const* folder)
  {
    int idx = 0;
    string new_name;
    do 
    {
      string filename = (idx > 0)
        ? str(format("%s (%d)") % name % idx)
        : name;
      new_name = str(format("%s\\%s") % folder % filename);
      idx++;
    } while (fs::exists(new_name));

    return new_name;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EMFFile::EMFFile(HFSVolume* volume, HFSPlusForkData const& fork
  , HFSCatalogNodeID fileID
  , HFSKey& filekey
  , bool deleted)
  : HFSFile(volume, fork, fileID, deleted)
{
  m_protection_version = volume->protection_version();
  m_decrypt_offset = 0;
  m_file_key = filekey;

  auto fk = m_file_key.as_buffer();
  
  uint8_t fk_[32] = { 0 };
  memcpy(fk_, fk, 32);

  if (m_protection_version == 4)
  {
    SHA_CTX ctx;
    SHA1_Init(&ctx);
    SHA1_Update(&ctx, fk_, 32);
    SHA1_Final(fk_, &ctx);

    m_ivkey.set_encrypt(fk_, 16);
  }
}

EMFFile::~EMFFile()
{
}

bool EMFFile::decrypt_file_to(string const& path, bool tr)
{
  ofstream ofs;
  ofs.open(path.c_str(), ios_base::binary);
  if (!ofs.is_open())
    return false;

  m_decrypt_offset = 0;
  auto bs = m_block_size;
  for (auto it=m_extents.begin(); it!=m_extents.end(); ++it)
  {
    for (uint32_t i=0; i<it->blockCount; ++i)
    {
      auto  lba = int64_t(it->startBlock) + i;
      auto from = lba * bs;
      ByteBuffer buffer = m_volume->read(from, bs);
      if (buffer.size() == bs)
      {
        process_block(lba, buffer, bs);
        ofs.write(buffer, buffer.size());
      }
    }
  }
  
  if (fs::file_size(path) > 0 && tr)
    fs::resize_file(path, m_logical_size);

  return true;
}

void EMFFile::decrypt_file()
{
  /**/
  m_decrypt_offset = 0;
  auto bs = m_block_size;
  for (auto it=m_extents.begin(); it!=m_extents.end(); ++it) 
  {
    for (uint32_t i=0; i<it->blockCount; ++i)
    {
      auto  lba = int64_t(it->startBlock) + i;
      auto from = lba * bs;
      ByteBuffer buffer = m_volume->read(from, bs);
      if (buffer.size() == bs)
      {
        process_block(lba, buffer, bs);
        m_volume->write(from, buffer);
      }
    }
  }

  /**/
}

void EMFFile::process_block(int64_t lba, ByteBuffer& buffer, uint32_t bs)
{
  auto v = dynamic_cast<EMFVolume*>(m_volume);

  uint32_t iv[4] = { 0 };
  uint8_t*   b = buffer;
  auto& emfkey = v->emfkey().as_aeskey();

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
  else
  {
    uint32_t iv_out[4] = { 0 };
    auto  size = (bs == m_block_size) ? 0x1000 : bs;
    auto& ivkey = m_ivkey.as_aeskey();

    for (uint32_t i=0; ; i++)
    {
      v->iv_for_lba((uint32_t)m_decrypt_offset, iv, false);
      AES_encrypt((const uint8_t*)iv, (uint8_t*)iv_out, &ivkey);
      AES_cbc_encrypt(b + i*size, b + i*size, size, &filekey, (uint8_t*)iv_out, AES_DECRYPT);
      m_decrypt_offset += size;
      if (bs != m_block_size || i >= bs/0x1000 - 1)
        break;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
