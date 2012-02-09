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
#include <boost/tuple/tuple.hpp>

#include <openssl/sha.h>

using namespace boost;
using namespace utility::parser;
      namespace fs = boost::filesystem;
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

void EMFVolume::undelete()
{
  auto journal = read_journal();

  auto res = carve_tree(journal);
  for (auto i=0; i<res.size(); i++)
    cout << res[i].data.file.fileID << endl; 
}

void EMFVolume::decrypt_all_files()
{
  m_not_encrypted.clear();
  m_decrypted_count = 0;
  m_active_files.clear();

  m_catalog_tree->traverse_leaf_nodes(bind(&EMFVolume::decrypt_file, this, _1));
}

bool EMFVolume::decrypt_file(CatalogRecord const& rec)
{
  if (rec.data.recordType != kHFSPlusFileRecord)
    return false;

  auto name = rec.key.nodeName.to_s();
  cout << name << endl;
  
  auto cprotect = m_attribute_tree->get_attribute(rec.data.file.fileID, "com.apple.system.cprotect");
  if (cprotect.empty())
  {
    m_not_encrypted.push_back(name);
    return false;
  }

  AES_KEY fk, ik;
  bool res;
  tie(res, fk, ik) = get_file_keys_for_cprotect(cprotect);
  
  if (!res)
    return false;
  
  EMFFile ef(this, rec.data.file.dataFork, rec.data.file.fileID, fk, ik);
  ef.decrypt_file();

  CatalogRecord f(kBTLeafNode);

  m_active_files.insert(rec);
  m_decrypted_count++;

  return true;
}

auto EMFVolume::iv_for_lba(uint32_t lba, uint32_t* iv, bool add) -> void
{
  if (add)
    lba += uint32_t(m_lba_offset);
    
  for (int i=0; i<4; i++)
  {
    lba = (lba & 1) ? 0x80000061 ^ (lba >> 1) : lba >> 1;
    iv[i] = lba;
  }
}

auto EMFVolume::get_file_keys_for_cprotect(ByteBuffer& cp) 
  -> ::boost::tuple<bool, AES_KEY, AES_KEY>
{
  AES_KEY file_key, iv_key; 
  bool res = true;
  
  auto v = protection_version();
  auto pwrapped_key = (v == 4)
    ? cprotect_xattr_v4(cp).persistent_key
    : cprotect_xattr_v2(cp).persistent_key;
  
  auto pclass = (v == 4)
    ? cprotect_xattr_v4(cp).persistent_class
    : cprotect_xattr_v2(cp).persistent_class;
  
  if (!unwrap_filekeys_for_class(pclass, pwrapped_key, file_key, iv_key))
    res = false;
  
  return ::boost::make_tuple(res, file_key, iv_key);
}

// 1. unwrap file key
// 2. get iv key if protection_class == 4
auto EMFVolume::unwrap_filekeys_for_class(uint32_t pclass, uint8_t* wrapped_key, AES_KEY& file_key, AES_KEY& ivkey)
  -> bool
{
  if (pclass < 1 || pclass >= MAX_CLASS_KEYS)
  {
    cerr << str(format("wrong protection class %d \n") % pclass);
    return false;
  }
  
  if ((m_class_keys_bitset & (1 << pclass)) == 0)
  {
    cerr << str(format("class key %d is not available\n") % pclass);
    return false;
  }
  
  uint8_t fk[32] = { 0 };
  auto wsize = AES_unwrap_key(&(m_class_keys[pclass-1]), NULL, fk, wrapped_key, 40);
  if (wsize != 32)
  {
    auto msg = (pclass == 2 && wsize == 0x48)
      ? "EMF unwrap curve25519 is not implemented yet"
      : str(format("EMF unwrap failed for protection class %d") % pclass);
    
    cerr << msg << "\n";
    
    return false;
  }
  
  AES_set_decrypt_key(fk, 32*8, &file_key);

  if (m_protect_version == 4)
  {
    SHA_CTX ctx;
    SHA1_Init(&ctx);
    SHA1_Update(&ctx, fk, 32);
    SHA1_Final(fk, &ctx);
    AES_set_encrypt_key(fk, 16*8, &ivkey);  // takes only the first 16 bytes
  }
  
  return true;  
}

auto EMFVolume::carve_tree(utility::hex::ByteBuffer& journal)
  -> vector<CatalogRecord>
{
  journal.flip();
  journal_header jh(journal);

  auto sector_size = jh.jhdr_size;
  auto   node_size = m_catalog_tree->node_size();

  CatalogNode files;

  std::vector<CatalogRecord> f0, f1, f2;
  for (uint32_t i=0; ; ++i)
  {
    auto beg = sector_size * (i + 1); // skip header
    auto end = beg + node_size;
    if (end >= journal.size())
      break;

    journal.offset(beg);
    carve_tree_node<CatalogRecord>(journal, beg, end, f0);
  }

  sort(f0.begin(), f0.end());
  unique_copy(f0.begin(), f0.end(), back_inserter(f1));
  set_difference(f1.begin(), f1.end(), m_active_files.begin(), m_active_files.end(), back_inserter(f2));

  return f2;
}

//
// TODO start, end의 데이타 타입이 uint32_t가 맞는지 확인
//
template <typename RecordT, typename NodeT>
void EMFVolume::carve_tree_node(utility::hex::ByteBuffer& journal, uint32_t start, uint32_t end, NodeT& node)
{
  auto base = start;

  try
  {
    BTNodeDescriptor btnode(journal);

    if (btnode.kind != kBTLeafNode || btnode.height != 1)
      return;

    auto offset_pos = end - 2 * btnode.numRecords;
    journal.offset(offset_pos);
    vector<uint16_t> offsets;
    for (auto i=0; i<btnode.numRecords; i++) offsets.push_back(journal.get_uint2_be());

    for (auto i=0; i<btnode.numRecords; i++)
    {
      auto pos = base + offsets[btnode.numRecords - i - 1];
      journal.offset(pos);

      try
      {
        RecordT rec(kBTLeafNode);
        rec.key.read_from(journal);

        auto type = journal.get_uint2_be();
        if (type != 2)                            // 2 == kHFSPlusFileRecord
          continue;

        journal.unget(2);
        rec.data.read_from(journal);
        node.push_back(rec);
      }
      catch(...)
      {
      }
    }
  }
  catch(...)
  {
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
