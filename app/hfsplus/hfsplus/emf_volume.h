#ifndef EMF_VOLUME_H_X03CV5QQ
#define EMF_VOLUME_H_X03CV5QQ

#include "hfs_volume.h"
#include "emf.h"

#include <openssl/aes.h>
#include <boost/tuple/tuple.hpp>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef BTreeRecord<HFSPlusCatalogKey, HFSPlusCatalogData> CatalogRecord;
typedef BTreeNode<CatalogRecord> CatalogNode;

class EMFVolume: public HFSVolume
{
public:
  EMFVolume(int64_t offset=0);
  virtual ~EMFVolume();

  virtual auto open(std::string const& filename) -> bool;

public:
  void decrypt_all_files();
  bool decrypt_file(CatalogRecord const& r);

  void undelete();
  
  auto iv_for_lba(uint32_t lba, uint32_t* iv, bool add=true) -> void;

  auto get_file_keys_for_cprotect(utility::hex::ByteBuffer& cp) 
    -> boost::tuple<bool, AES_KEY, AES_KEY>;
  
  auto unwrap_filekeys_for_class(uint32_t pclass, uint8_t* wrapped_key, AES_KEY& fkey, AES_KEY& ikey)
    -> bool;

  virtual auto protection_version() 
    -> int16_t  { return m_protect_version; }
  
  auto emfkey() 
    -> AES_KEY& { return m_emfkey; }

private:
  auto carve_tree_node(utility::hex::ByteBuffer& journal)
    -> CatalogNode;

private:
  uint16_t m_protect_version;
  uint64_t m_lba_offset;
  uint32_t m_class_keys_bitset;
  AES_KEY  m_emfkey;
  AES_KEY  m_class_keys[MAX_CLASS_KEYS];
  HFSCatalogNodeID m_metadata_dir;

private:
  std::vector<std::string> m_not_encrypted;
  uint32_t m_decrypted_count;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
