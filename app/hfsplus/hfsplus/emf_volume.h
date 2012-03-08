#ifndef EMF_VOLUME_H_X03CV5QQ
#define EMF_VOLUME_H_X03CV5QQ

#include "hfs_volume.h"
#include "emf.h"
#include "hfs_key.h"

#include <boost/function.hpp>
#include <boost/regex.hpp>

#include <map>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef BTreeRecord<HFSPlusCatalogKey, HFSPlusCatalogData> CatalogRecord;
typedef BTreeNode<CatalogRecord> CatalogNode;

struct CarvePoint
{
  CarvePoint()
    :lba(0), key(nullptr), id(0)  
  {}

  CarvePoint(uint32_t lba_, HFSKey const* key_, int id_)
    :lba(lba_), key(key_), id(id_)  
  {}

  uint32_t lba;
  HFSKey const* key;
  int id;
};

typedef std::vector<CarvePoint> CarvePoints;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Signature;

typedef boost::function<bool(uint32_t)> UnusedAreaVisitor;

class EMFVolume: public HFSVolume
{
public:
  EMFVolume();
  virtual ~EMFVolume();

  virtual auto open(std::string const& filename) -> bool;

public:
  void decrypt_all_files();
  bool decrypt_file(CatalogRecord const& r);

  void undelete();
  
  auto iv_for_lba(uint32_t lba, uint32_t* iv, bool add=true) -> void;

  auto get_file_keys_for_cprotect(utility::hex::ByteBuffer& cp) 
    -> std::pair<bool, HFSKey>;
  
  auto unwrap_filekeys_for_class(uint32_t pclass, uint8_t* wrapped_key)
    -> std::pair<bool, HFSKey>;

  void carve_data_to(std::string const& s);

  // void carve_node_slacks_to(std::string const& s);

  virtual auto protection_version() 
    -> int16_t  { return m_protect_version; }
  
  auto emfkey() 
    -> HFSKey&  { return m_emfkey; }
  
  auto traverse_unused_area(UnusedAreaVisitor) -> bool;

private:
  void undelete_based_on_journal(ByteBuffer& jnl
      , std::vector<CatalogRecord>& files
      , std::map<uint32_t, HFSKeys>&
      , std::string const&);

  void undelete_unused_area_using(HFSKeys const& keys, std::string const&);

  auto undelete_unused_area_using(std::string const& pattern, std::string const&)
    -> void;
  
  auto carve_journal(utility::hex::ByteBuffer& journal)
    -> std::pair<std::vector<CatalogRecord>, std::map<uint32_t, HFSKeys>>;

  
  template <typename RecordT, typename NodeT>
  auto carve_tree_node(ByteBuffer& journal, uint32_t node_size)
    -> NodeT;

  auto carve_lba(uint32_t lba, HFSKey const& key, boost::regex const&) -> bool;
  
  auto carve_unused_area(uint32_t slba, uint32_t elba, HFSKey const& key, Signature* sig)
    -> ByteBuffer;

private:
  uint16_t m_protect_version;
  uint64_t m_lba_offset;
  HFSKey   m_emfkey;
  uint32_t m_class_keys_bitset;
  std::vector<HFSKey> m_class_keys;
  HFSCatalogNodeID m_metadata_dir;

private:
  std::string m_carve_dir;
  std::vector<std::string> m_not_encrypted;
  std::set<CatalogRecord> m_active_files;
  std::map<std::string, HFSKey> m_active_file_keys;
  std::set<uint32_t> m_decrypted_lbas;

  uint32_t m_decrypted_count;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
