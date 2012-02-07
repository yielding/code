#include "emf_file.h"
#include "emf_volume.h"

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
  m_decrypt_offset = 0;
  auto bs = block_size();
  for (auto it=m_extents.begin(); it !=m_extents.end(); ++it)
  {
    for (uint32_t i=0; i<it->blockCount; ++i)
    {
      auto  lba = int64_t(it->startBlock) + i;
      auto from = lba * bs;
      ByteBuffer buffer = m_volume->read(from, bs);
      if (buffer.size() == bs)
      {
        process_block(lba, buffer);
        m_volume->write(from, buffer);
      }
    }
  }
}

void EMFFile::process_block(int64_t lba, ByteBuffer& buffer)
{
  auto v = dynamic_cast<EMFVolume*>(m_volume);
  
  uint32_t iv[4] = { 0 };
  uint8_t*   b = buffer;
  auto& emfkey = v->emfkey();

  v->iv_for_lba(uint32_t(lba), iv);
  AES_cbc_encrypt(b, b, m_block_size, &emfkey, (uint8_t*)iv, AES_ENCRYPT);
  
  if (v->protection_version() == 0)
  {
    v->iv_for_lba(uint32_t(lba), iv);
    AES_cbc_encrypt(buffer, buffer, m_block_size, &m_file_key, (uint8_t*)iv, AES_DECRYPT);
  }
  else
  {
    uint32_t iv_out[4] = { 0 };
    for (uint32_t i=0; i<m_block_size/0x1000; i++)
    {
      v->iv_for_lba((uint32_t)m_decrypt_offset, iv, false);
      AES_encrypt((const uint8_t*)iv, (uint8_t*)iv_out, &m_ivkey);
      AES_cbc_encrypt(b + i * 0x1000, 
                      b + i * 0x1000, 0x1000, &m_file_key, (uint8_t*)iv_out, AES_DECRYPT);
      m_decrypt_offset += 0x1000;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
