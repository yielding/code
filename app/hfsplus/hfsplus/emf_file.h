#ifndef EMF_FILE_H_2N1X36JD
#define EMF_FILE_H_2N1X36JD

#include "hfs_file.h"

#include <openssl/aes.h>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSVolume;

class EMFFile: public HFSFile
{
public:
  EMFFile(HFSVolume* volume, HFSPlusForkData const& fork, HFSCatalogNodeID fileID
    , AES_KEY const& filekey, AES_KEY const& ivkey, bool deleted=false);

  virtual ~EMFFile();

public:
  void decrypt_file();
  void process_block(int64_t lba, utility::hex::ByteBuffer& buffer, uint32_t bs=0);

private:
  int64_t  m_decrypt_offset;
  uint16_t m_protection_version;
  AES_KEY  m_file_key, m_ivkey;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
