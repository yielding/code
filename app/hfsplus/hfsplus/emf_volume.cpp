#include "emf_volume.h"
#include "catalog_btree.h"
#include "attribute_btree.h"
#include "emf_file.h"
#include "PtreeParser.h"
#include "HexUtil.h"

#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <boost/bind.hpp>

namespace fs = boost::filesystem;
using namespace boost;
using namespace utility::parser;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EMFVolume::EMFVolume(int64_t offset)
  : HFSVolume(offset)
  , m_protect_version(0)
  , m_lba_offset(0)
  , m_class_keys_bitset(0)
{
}

EMFVolume::~EMFVolume()
{
}

auto EMFVolume::open(std::string const& filename) -> bool
{
  if (!HFSVolume::open(filename))
    return false;

  m_metadata_dir = m_catalog_tree->metadata_dir_id();
  auto buffer = m_attribute_tree->get_attribute(kHFSRootParentID, "com.apple.system.cprotect");
  if (buffer.size() <=0)
    return false;
  
  cp_root_xattr xattr(buffer);
  m_protect_version = xattr.major_version;
  
  // Key file initialize
  fs::path p(filename); p.remove_leaf() /= str(format("%x.plist") % id());
  auto path = p.string();
  if (!fs::exists(path))
      throw std::runtime_error("key file does not exist");
  
  PTreeParser ptree;
  if (!ptree.init_with_path(path))
    throw std::runtime_error("key file read error");
  
  ptree.in("plist.dict");
  string emf = ptree.get_string("EMF");
  if (emf.empty())
    throw std::runtime_error("emf key does not exist in the key file");

  auto emf_bytes = utility::Hex::bytes_from_hexcode(emf);
  AES_set_encrypt_key(&emf_bytes[0], 32*8, &m_emfkey);

  string dkey = ptree.get_string("DKey");
  if (dkey.empty())
    throw std::runtime_error("dkey key does not exist in the key file");

  auto dkey_bytes = utility::Hex::bytes_from_hexcode(dkey); 
  AES_set_decrypt_key(&dkey_bytes[0], 32*8, &m_class_keys[CLASS_DKEY-1]);
  m_class_keys_bitset |= 1 <<CLASS_DKEY;
  
  auto class_keys = ptree.get_dict("classKeys");
  for (size_t i=0; i<class_keys.size(); ++i)
  {
    auto klass = lexical_cast<int>(class_keys[i].first);
    auto value = class_keys[i].second;
    if (klass > 0 && klass <= MAX_CLASS_KEYS && value.length() == 64)
    {
      auto class_key_bytes = utility::Hex::bytes_from_hexcode(value);
      assert(class_key_bytes.size() == 32);
      AES_set_decrypt_key(&class_key_bytes[0], 32*8, &(m_class_keys[klass-1]));
      m_class_keys_bitset |= 1 << klass;
    }
  }
  
  m_lba_offset = ptree.get_int("dataVolumeOffset");
  if (m_lba_offset == -1)
    throw std::runtime_error("lbaoffset key does not exist in the key file");
    
  return true;
}

void EMFVolume::decrypt_all_files()
{
  m_not_encrypted.clear();
  m_decrypted_counnt = 0;
  m_catalog_tree->traverse_leaf_nodes(boost::bind(&EMFVolume::decrypt_file, this, _1));
}

bool EMFVolume::decrypt_file(CatalogRecord const& rec)
{
  if (rec.data.recordType != kHFSPlusFileRecord)
    return false;

  auto name = rec.key.nodeName.to_s();
  auto cprotect = m_attribute_tree->get_attribute(rec.data.file.fileID, "com.apple.system.cprotect");
  if (cprotect.empty())
  {
    m_not_encrypted.push_back(name);
    return false;
  }

  auto fk = get_file_key_for_cprotect(cprotect);
  
  EMFFile ef(this, rec.data.file.dataFork, rec.data.file.fileID, fk);
  ef.decrypt_file();
  
  m_decrypted_counnt++;

  return true;
}

auto EMFVolume::get_file_key_for_cprotect(ByteBuffer const& cp) -> AES_KEY
{
  AES_KEY file_key;
  
  return file_key;
}


////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
