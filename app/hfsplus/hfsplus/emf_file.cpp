#include "emf_file.h"
#include "emf_volume.h"

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
  , AES_KEY const& filekey, AES_KEY const& ivkey
  , bool deleted)
  : HFSFile(volume, fork, fileID, deleted)
{
  m_protection_version = volume->protection_version();
  m_decrypt_offset = 0;
  m_file_key = filekey;
  m_ivkey = ivkey;
}

EMFFile::~EMFFile()
{
}

void EMFFile::decrypt_file()
{
  /*
  m_decrypt_offset = 0;
  auto bs = m_block_size;
  for (auto it=m_extents.begin(); it !=m_extents.end(); ++it)
  {
    for (uint32_t i=0; i<it->blockCount; ++i)
    {
      auto  lba = int64_t(it->startBlock) + i;
      auto from = lba * bs;
      ByteBuffer buffer = m_volume->read(from, bs);
      if (buffer.size() == bs)
      {
        process_block(lba, buffer, bs);
        // m_volume->write(from, buffer);
      }
    }
  }
  */
}

void EMFFile::process_block(int64_t lba, ByteBuffer& buffer, uint32_t bs)
{
  auto v = dynamic_cast<EMFVolume*>(m_volume);

  uint32_t iv[4] = { 0 };
  uint8_t*   b = buffer;
  auto& emfkey = v->emfkey();

  // 1. re-encrypt using emf key
  v->iv_for_lba(uint32_t(lba), iv);
  AES_cbc_encrypt(b, b, bs, &emfkey, (uint8_t*)iv, AES_ENCRYPT);

  // 2. decrypt with filekey
  if (v->protection_version() == 0)
  {
    v->iv_for_lba(uint32_t(lba), iv);
    AES_cbc_encrypt(buffer, buffer, bs, &m_file_key, (uint8_t*)iv, AES_DECRYPT);
  }
  else
  {
    uint32_t iv_out[4] = { 0 };
    auto size = (bs == m_block_size) ? 0x1000 : bs;

    for (uint32_t i=0; ; i++)
    {
      v->iv_for_lba((uint32_t)m_decrypt_offset, iv, false);
      AES_encrypt((const uint8_t*)iv, (uint8_t*)iv_out, &m_ivkey);
      AES_cbc_encrypt(b + i*size, b + i*size, size, &m_file_key, (uint8_t*)iv_out, AES_DECRYPT);
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
