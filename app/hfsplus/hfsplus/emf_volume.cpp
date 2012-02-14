#include "emf_volume.h"
#include "catalog_btree.h"
#include "attribute_btree.h"
#include "emf_file.h"
#include "PtreeParser.h"
#include "HexUtil.h"

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <boost/bind.hpp>

#include <openssl/sha.h>
#include <cassert>

using namespace std;
using namespace boost;
using namespace utility::parser;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  struct not_a_catalog_record
  {
    template <typename Record>
    bool operator()(Record const& r)
    {
      return r.data.recordType != 2;
    }
  };

  std::string get_new_filepath(string const&name, string const& folder)
  {
    int idx = 0;
    string new_name;
    do 
    {
      string filename = (idx > 0)
      ? str(format("%s (%d)") % name % idx)
      : name;
      new_name = str(format("%s/%s") % folder % filename);
      idx++;
    } while (fs::exists(new_name));
    
    return new_name;
  }
  

  bool decrypted_correctly(ByteBuffer& b)
  {
    char const* magics[] = { 
      "SQLite", "bplist", "<?xml", "\xFF\xD8\xFF", "\xCE\xFA\xED\xFE",
      "GIF87a", "GIF89a", "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"
    };
    
    if (b.size() < 3)
      return false;
    
    int size = sizeof(magics)/sizeof(magics[0]);
    for (int i=0; i<size; i++)
    {
      string m(magics[i]);
      size_t size = size_t(b.size() < 10 ? b.size() : 10);
      string ss((char const*)b, size);
      
      if (starts_with(ss, m))
        return true;
    }
    
    // mpeg4 test
    if (b.size() < 8)
      return false;
    
    uint8_t mpeg4[] = { 0x66, 0x74, 0x79, 0x70 };
    auto size2 = sizeof(mpeg4) / sizeof(mpeg4[0]);
    auto buffer = ((uint8_t*)b) + 4;
    bool found  = false;
    for (int i=0; i<(int)size2; i++)
    {
      if (buffer[i] != mpeg4[i])
      {
        found = true;
        break;
      }
    }
    
    return !found;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EMFVolume::EMFVolume()
  : HFSVolume(0)
  , m_protect_version(0)
  , m_lba_offset(0)
  , m_class_keys_bitset(0)
  , m_class_keys(MAX_CLASS_KEYS)
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
  m_emfkey.set_encrypt(&emf_bytes[0]);

  string dkey = ptree.get_string("DKey");
  if (dkey.empty())
    throw std::runtime_error("dkey key does not exist in the key file");

  auto dkey_bytes = utility::Hex::bytes_from_hexcode(dkey); 
  m_class_keys[CLASS_DKEY-1].set_decrypt(&dkey_bytes[0]);
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
      m_class_keys[klass-1].set_decrypt(&class_key_bytes[0]);
      m_class_keys_bitset |= 1 << klass;
    }
  }
  
  m_lba_offset = ptree.get_int("dataVolumeOffset");
  if (m_lba_offset == -1)
    throw std::runtime_error("lbaoffset key does not exist in the key file");

  return true;
}

void EMFVolume::undelete(string const& undeletePath)
{
  auto node_size = m_catalog_tree->node_size();
  auto b = m_catalog_tree->read_empty_space();
  auto node_count = b.size() / node_size;
  
  for (uint32_t i=0; i<node_count; i++)
  {
    std::vector<CatalogRecord> f1, f2, f3;
    b.offset(i*node_size);
    auto f0 = carve_tree_node<CatalogRecord, vector<CatalogRecord>>(b, node_size);
    if (f0.size() > 0)
    {
      sort(f0.begin(), f0.end());
      remove_copy_if(f0.begin(), f0.end(), back_inserter(f1), not_a_catalog_record());
    }
  }
  

  /*
  auto jnl = read_journal();
  auto res = carve_journal(jnl);
  auto files = res.first;
  auto key_map = res.second;

  // use this cache to prevent double decryption of same allocation block
  m_decrypted_lbas.clear();
  
  for (auto it=files.begin(); it!=files.end(); ++it)
  {
    auto key  = it->key;
    auto data = it->data;

    auto keys = key_map[data.file.fileID];
    if (keys.empty())
      continue;

    auto name = key.nodeName.to_s();
    auto to_save = get_new_filepath(name.c_str(), undeletePath.c_str());
    // cout << to_save << endl;
    for (auto jt=keys.begin(); jt!=keys.end();)
    {
      EMFFile ef(this, data.file.dataFork, data.file.fileID, *jt);
      auto buffer = ef.read_all_to_buffer();
      if (decrypted_correctly(buffer))
      {
        ofstream ofs;
        ofs.open(to_save.c_str(), ios_base::binary);
        ofs.write(buffer, buffer.size());
        m_decrypted_lbas.insert(ef.start_lba());
        
        keys.erase(jt++);
      }
      else
      {
        ++jt;
      }
    }
  }
  */
}

void EMFVolume::decrypt_all_files()
{
  m_not_encrypted.clear();
  m_decrypted_count = 0;
  m_active_files.clear();
  m_active_file_keys.clear();

  m_catalog_tree->traverse_leaf_nodes(bind(&EMFVolume::decrypt_file, this, _1));
}

bool EMFVolume::decrypt_file(CatalogRecord const& rec)
{
  if (rec.data.recordType != kHFSPlusFileRecord)
    return false;

  auto name = rec.key.nodeName.to_s();
  // TODO: Debug
  // cout << name << endl;
  
  auto cprotect = m_attribute_tree->get_attribute(rec.data.file.fileID, 
                                                 "com.apple.system.cprotect");
  if (cprotect.empty())
  {
    m_not_encrypted.push_back(name);
    return false;
  }

  HFSKey fk;
  bool res;
  tie(res, fk) = get_file_keys_for_cprotect(cprotect);
  
  if (!res)
    return false;
  
//  EMFFile ef(this, rec.data.file.dataFork, rec.data.file.fileID, fk);
//  ef.decrypt_file();

  m_active_files.insert(rec);
  m_active_file_keys.insert(fk);
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
  -> pair<bool, HFSKey>
{
  auto v = protection_version();
  
  auto pwrapped_key = (v == 4)
    ? cprotect_xattr_v4(cp).persistent_key
    : cprotect_xattr_v2(cp).persistent_key;
  
  auto pclass = (v == 4)
    ? cprotect_xattr_v4(cp).persistent_class
    : cprotect_xattr_v2(cp).persistent_class;
  
  return (pwrapped_key.size() == 40)
    ? unwrap_filekeys_for_class(pclass, pwrapped_key)
    : make_pair(false, HFSKey());
}

//
// 1. unwrap file key
// 2. get iv key if protection_class == 4
//
auto EMFVolume::unwrap_filekeys_for_class(uint32_t pclass, uint8_t* wrapped_key)
  -> pair<bool, HFSKey>
{
  HFSKey filekey;

  if (pclass < 1 || pclass >= MAX_CLASS_KEYS)
  {
    cerr << str(format("wrong protection class %d \n") % pclass);
    return make_pair(false, filekey);
  }
  
  if ((m_class_keys_bitset & (1 << pclass)) == 0)
  {
    cerr << str(format("class key %d is not available\n") % pclass);
    return make_pair(false, filekey);
  }
  
  uint8_t fk[32] = { 0 };
  auto k = m_class_keys[pclass-1].as_aeskey();
  auto wsize = AES_unwrap_key(&k, NULL, fk, wrapped_key, 40);
  if (wsize != 32)
  {
    auto msg = (pclass == 2 && wsize == 0x48)
      ? "EMF unwrap curve25519 is not implemented yet"
      : str(format("EMF unwrap failed for protection class %d") % pclass);
    
    cerr << msg << "\n";
    
    return make_pair(false, filekey);
  }
  
  filekey.set_decrypt(fk);
  return make_pair(true, filekey);
}

auto EMFVolume::carve_journal(ByteBuffer& journal)
  -> pair<vector<CatalogRecord>, map<uint32_t, HFSKeys>>
{
  auto node_size = m_catalog_tree->node_size();

  std::vector<CatalogRecord> f1, f2, f3;
  {
    auto f0 = carve_emf_journal<CatalogRecord, vector<CatalogRecord>>(journal, node_size);
    sort(f0.begin(), f0.end());
    remove_copy_if(f0.begin(), f0.end(), back_inserter(f1), not_a_catalog_record());
  }

  unique_copy(f1.begin(), f1.end(), back_inserter(f2));
  set_difference(f2.begin(), f2.end(), 
      m_active_files.begin(), m_active_files.end(), 
      back_inserter(f3));

  auto a0 = carve_emf_journal<AttrRecord, vector<AttrRecord>>(journal, node_size);

  map<uint32_t, HFSKeys> fks;
  for (auto it=a0.begin(); it!=a0.end(); ++it)
  {
    if (it->key.name.to_s() != string("com.apple.system.cprotect"))
      continue;

    if (it->data.data.size() <= 0)
      continue;

    auto rec = m_catalog_tree->search_by_cnid(it->key.fileID);
    if (!rec.empty()) // not exist in catalog tree
      continue;
    
    HFSKey fk; 
    bool   key_exists;
    ByteBuffer b(&it->data.data[0], it->data.data.size());
    tie(key_exists, fk) = get_file_keys_for_cprotect(b);

    if (!key_exists)
      continue;

    fks[it->key.fileID].insert(fk);
  }
        
  return make_pair(f3, fks);
}

template <typename RecordT, typename NodeT>
auto EMFVolume::carve_tree_node(ByteBuffer& buffer, uint32_t sz)
  -> NodeT
{
  NodeT result;

  auto beg = buffer.offset();
  auto end_ = beg + sz;
  try
  {
    BTNodeDescriptor btnode(buffer);

    if (btnode.kind != kBTLeafNode || btnode.height != 1)
      return result;

    auto offset_pos = end_ - 2*btnode.numRecords;
    buffer.offset(offset_pos);
    vector<uint16_t> offsets;
    for (auto i=0; i<btnode.numRecords; i++) offsets.push_back(buffer.get_uint2_be());

    for (auto i=0; i<btnode.numRecords; i++)
    {
      auto pos = beg + offsets[btnode.numRecords - i - 1];
      buffer.offset(uint32_t(pos));

      try
      {
        RecordT rec(kBTLeafNode);
        rec.key.read_from(buffer);
        if (!rec.key.ok())
          continue;

        rec.data.read_from(buffer);
        result.push_back(rec);
      }
      catch(...)
      {
      }
    }
  }
  catch(...)
  {
  }

  return result;
}

//
// TODO start, end의 데이타 타입이 uint32_t가 맞는지 확인
//
template <typename RecordT, typename NodeT>
auto EMFVolume::carve_emf_journal(ByteBuffer& journal, uint32_t node_size)
  -> NodeT
{
  NodeT result;
  journal.flip();

  for (uint32_t i=0; ; i++)
  {
    auto beg = m_sector_size * (i + 1); // skip header
    auto end = beg + node_size;
    if (end >= journal.size())
      break;

    journal.offset(beg);
    try
    {
      BTNodeDescriptor btnode(journal);

      if (btnode.kind != kBTLeafNode || btnode.height != 1)
        continue;

      auto offset_pos = end - 2*btnode.numRecords;
      journal.offset(offset_pos);
      vector<uint16_t> offsets;
      for (auto i=0; i<btnode.numRecords; i++) offsets.push_back(journal.get_uint2_be());

      for (auto i=0; i<btnode.numRecords; i++)
      {
        auto pos = beg + offsets[btnode.numRecords - i - 1];
        journal.offset(pos);

        try
        {
          RecordT rec(kBTLeafNode);
          rec.key.read_from(journal);
          if (!rec.key.ok())
            continue;

          rec.data.read_from(journal);
          result.push_back(rec);
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

  return result;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
