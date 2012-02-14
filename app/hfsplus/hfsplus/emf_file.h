#ifndef EMF_FILE_H_2N1X36JD
#define EMF_FILE_H_2N1X36JD

#include "hfs_file.h"
#include "emf_volume.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class EMFFile: public HFSFile
{
public:
  EMFFile(HFSVolume* volume, HFSPlusForkData const& fork_data
    , HFSCatalogNodeID fileID 
    , HFSKey const& filekey
    , bool deleted=false);

  virtual ~EMFFile();

public:
  void decrypt_file();
  bool decrypt_partial();
  bool decrypt_file_to(std::string const& path, bool tr=true);
  
public:
  uint32_t start_lba();
  
private:
  virtual auto process_block(int64_t lba, utility::hex::ByteBuffer& buffer, uint32_t bs=0)
    -> ByteBuffer&;

private:
  int64_t  m_decrypt_offset;
  uint16_t m_protection_version;
  HFSKey   m_file_key, m_ivkey;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
