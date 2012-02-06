#ifndef EMF_VOLUME_H_X03CV5QQ
#define EMF_VOLUME_H_X03CV5QQ

#include "hfs_volume.h"
#include "emf.h"

#include <openssl/aes.h>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef BTreeRecord<HFSPlusCatalogKey, HFSPlusCatalogData> CatalogRecord;

class EMFVolume: public HFSVolume
{
public:
  EMFVolume(int64_t offset=0);
  virtual ~EMFVolume();

  virtual auto open(std::string const& filename) -> bool;

public:
  void decrypt_all_files();
  bool decrypt_file(CatalogRecord const& r);
  
  /*
  void read_file(std::string const& path);
  
  void iv_for_lba(uint32_t lba);
   */

  virtual auto protection_version() -> int16_t
  {
    return m_protect_version;
  }

  auto get_file_key_for_cprotect(utility::hex::ByteBuffer const& cp) -> AES_KEY;

private:
  uint16_t m_protect_version;
  uint64_t m_lba_offset;
  uint32_t m_class_keys_bitset;
  AES_KEY  m_emfkey;
  AES_KEY  m_class_keys[MAX_CLASS_KEYS];
  HFSCatalogNodeID m_metadata_dir;

private:
  std::vector<std::string> m_not_encrypted;
  uint32_t m_decrypted_counnt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
