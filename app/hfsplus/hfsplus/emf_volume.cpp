#include "emf_volume.h"
#include "catalog_btree.h"
#include "attribute_btree.h"
#include "emf_file.h"
#include "PtreeParser.h"
#include "HexUtil.h"
#include "signature.h"

#include <boost/algorithm/string.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>
#include <boost/bind.hpp>
#include <boost/format.hpp>

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
namespace 
{
  struct not_a_catalog_record
  {
    template <typename Record>
    bool operator()(Record const& r)
    {
      return r.data.recordType != 2;
    }
  };

  string get_new_filepath(string const&name, string const& folder)
  {
    int idx = 0;  
    string new_name;

    do 
    {
      auto filename = (idx > 0)
        ? str(format("%s (%d)") % name % idx)
        : name;
      new_name = str(format("%s/%s") % folder % filename);
      idx++;
    } while (fs::exists(new_name));
    
    return new_name;
  }

  void write_file(string const& path, ByteBuffer& buffer, 
                  ios_base::openmode mode=ios_base::binary)
  {
    ofstream ofs;
    ofs.open(path.c_str(), mode);
    ofs.write(buffer, buffer.size());
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EMFVolume::EMFVolume()
  : m_protect_version(0)
  , m_lba_offset(0)
  , m_class_keys_bitset(0)
  , m_class_keys(MAX_CLASS_KEYS)
  , m_carve_dir("")
{}

EMFVolume::~EMFVolume()
{
}

auto EMFVolume::open(std::string const& filename) -> bool
{
  if (!HFSVolume::open(filename))
    return false;

  m_metadata_dir = m_catalog_tree->metadata_dir_id();
  auto buffer = m_attribute_tree->get_attribute(kHFSRootParentID, 
      "com.apple.system.cprotect");
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

void EMFVolume::undelete_based_on_journal(ByteBuffer& jnl
    , vector<CatalogRecord>& files
    , map<uint32_t, HFSKeys>& key_map
    , string const& path)
{
  for (auto it=files.begin(); it!=files.end(); ++it)
  {
    auto key  = it->key;
    auto data = it->data;

    auto keys = key_map[data.file.fileID];
    if (keys.empty())
      continue;

    auto name = key.nodeName.to_s();
    auto to_save = get_new_filepath(name.c_str(), path.c_str());
    // cout << to_save << endl;
    for (auto jt=keys.begin(); jt!=keys.end();)
    {
      EMFFile ef(this, data.file.dataFork, data.file.fileID, *jt);
      auto buffer = ef.read_all_to_buffer();
      if (ef.decrypted_correctly(buffer))
      {
        write_file(to_save, buffer);
        m_decrypted_lbas.insert(ef.start_lba());
        
        keys.erase(jt++);
      }
      else
      {
        ++jt;
      }
    }
  }
}

void EMFVolume::undelete_unused_area_using(HFSKeys const& keys_
    , string const& save_path)
{
  typedef boost::shared_ptr<Signature> signature_ptr;
  vector<signature_ptr> sv;
  
  sv.push_back(signature_ptr(new JPEGSignature()));
//  sv.push_back(signature_ptr(new SQLiteSignature()));
//  sv.push_back(signature_ptr(new BPlistSignature()));
  
  CarvePoints pts;

  vector<HFSKey> keys;
  for (auto it=keys_.begin(); it!=keys_.end(); ++it)
    keys.push_back(*it);
  
  // 1. signature position/key/type 획득
  auto start = uint32_t((m_journal_offset + m_journal_size) / m_block_size);
  for (uint32_t lba=start; lba<m_header.totalBlocks; ++lba)
  {
    if (block_in_use(uint32_t(lba)))
      continue;

    if (m_decrypted_lbas.find(lba) != m_decrypted_lbas.end())
      continue;

    auto bf = read(lba * m_block_size, 16);
    auto kb = keys.begin();
    auto ke = keys.end();
    for (; kb != ke; ++kb)
    {
      auto b = bf;
      EMFFile ef(this, *kb, m_protect_version);
      ef.decrypt_buffer(lba, b);

      // TODO replace vector<signature> with map<signature>
      // and use phoenix with at_c expression
      auto it = find_if(sv.begin(), sv.end(), bind(&Signature::match_head, _1, b));
      if (it == sv.end())
        continue;
      
      pts.push_back(CarvePoint(lba, &*kb, (*it)->id()));
    }
  }
  
  // 2. signature carving
  for (uint32_t i=0; i<pts.size(); i++)
  {
    auto slba = pts[i].lba;
    auto elba = (pts.size() == 1) ? slba + 100 : pts[i+1].lba;
    auto   id = pts[i].id;
    auto  key = pts[i].key;
    auto  res = carve_unused_area(slba, elba, *key, sv[id].get());
    if (!res.empty())
    {
      // 1. indivisual file
      auto to_save = str(format("%s/%d.bin") % m_carve_dir % slba);
      write_file(to_save, res);
      // 2. update image
      // m_volume->write(slba*m_block_size, res);

      // 3. update cache
      m_decrypted_lbas.insert(slba);
    }
  }
}

void EMFVolume::undelete_unused_area_using(string const& pattern, string const& fn)
{
  if (m_active_file_keys.find(fn) == m_active_file_keys.end())
    return;

  HFSKey const& key = m_active_file_keys[fn];
  regex expr(pattern);
  bool interrupted = traverse_unused_area(bind(&EMFVolume::carve_lba, 
                                               this, _1, key, expr));
  if (interrupted)
  {
    // diagnosis
  }
}

auto EMFVolume::carve_lba(uint32_t lba, HFSKey const& key, regex const& expr)
  -> bool
{
  try
  {
    auto buffer = read(lba * m_block_size, m_block_size);
    EMFFile ef(this, key, m_protect_version);
    ef.decrypt_buffer(lba, buffer);

    auto page = (uint8_t*)buffer;
    if (page[0] == 0x0d)
    {
      cmatch what;
      if (regex_match((const char*)page, what, expr))
      {
        auto to_save = str(format("%s/%d_pattern.bin") % m_carve_dir % lba);
        write_file(to_save, buffer); 
      }
    }
  }
  catch(...)
  {
    return false;
  }

  return true;
}

auto EMFVolume::carve_unused_area(uint32_t slba, uint32_t elba, 
                                  HFSKey const& key, Signature* sig)
  -> ByteBuffer
{
  ByteBuffer carved;
  
  auto buffer = read(slba*m_block_size, m_block_size);

  EMFFile ef(this, key, m_protect_version);
  ef.decrypt_buffer(slba, buffer);
  carved.append(buffer);
  
  auto count = sig->head_count(buffer);
  if (count < 1)
    return ByteBuffer();
    
  if (sig->match_tail(buffer))
    count--;

  while (count > 0 && ++slba < elba)
  {
    auto b0 = read(slba*m_block_size, m_block_size);
    EMFFile ef0(this, key, m_protect_version);
    ef0.decrypt_buffer(slba, buffer);
    carved.append(b0);
    if (sig->match_tail(b0))
      count--;
  }

  return carved;
}

void EMFVolume::undelete()
{
  // use this cache to prevent double decryption of same allocation block
  m_decrypted_lbas.clear();

  auto jnl     = read_journal();
  auto carved  = carve_journal(jnl);
  auto files   = carved.first;
  auto key_map = carved.second;
  
  undelete_based_on_journal(jnl, files, key_map, m_carve_dir);

  HFSKeys keys;
  for (auto it=key_map.begin(); it!=key_map.end(); ++it)
  {
    auto& key_set = it->second;
    for (auto jt=key_set.begin(); jt!=key_set.end(); ++jt) keys.insert(*jt);
  }

  //
  // REMARK
  // We should carve more keys from other positions if possible
  // but it has turned out that there are no keys available in catalog and attribute tree
  // 
  undelete_unused_area_using(keys, m_carve_dir);

  // 
  // regular expression based data carving
  // 1. SMS (sms.db, sms.db-wal, iNode248, iNode3233)
  // auto const& pattern = "(01[016789]{1}|02|0[3-9]{1}[0-9]{1})-?[0-9]{3,4}-?[0-9]{4}";
  // undelete_unused_area_using(pattern, "sms.db");  // sms.db
}

void EMFVolume::decrypt_all_files()
{
  m_decrypted_count = 0;
  m_not_encrypted.clear();
  m_active_files .clear();
  m_active_file_keys.clear();

  m_catalog_tree->traverse_leaf_nodes(bind(&EMFVolume::decrypt_file, this, _1));
}

bool EMFVolume::decrypt_file(CatalogRecord const& rec)
{
  if (rec.data.recordType != kHFSPlusFileRecord)
    return false;

  auto name = rec.key.nodeName.to_s();
  // REMARK: for Debug
  // cout << name << endl;
  
//  if (starts_with(name, "sms"))
//  {
//    HFSPlusCatalogFile f = rec.data.file;
//  }
    
  auto cprotect = m_attribute_tree->get_attribute(rec.data.file.fileID, 
                                                 "com.apple.system.cprotect");
  if (cprotect.empty())
  {
    m_not_encrypted.push_back(name);
    return false;
  }

  HFSKey fk; bool res;
  tie(res, fk) = get_file_keys_for_cprotect(cprotect);
  
  if (!res)
    return false;
  
  EMFFile ef(this, rec.data.file.dataFork, rec.data.file.fileID, fk);
  ef.decrypt_file();

  m_active_files.insert(rec);
  m_active_file_keys[name] = fk;
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
    auto f0 = carve_tree_node<CatalogRecord, vector<CatalogRecord>>(journal, node_size);
    sort(f0.begin(), f0.end());
    remove_copy_if(f0.begin(), f0.end(), back_inserter(f1), not_a_catalog_record());
  }

  unique_copy(f1.begin(), f1.end(), back_inserter(f2));
  set_difference(f2.begin(), f2.end(), 
      m_active_files.begin(), m_active_files.end(), 
      back_inserter(f3));

  auto a0 = carve_tree_node<AttrRecord, vector<AttrRecord>>(journal, node_size);

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
auto EMFVolume::carve_tree_node(ByteBuffer& journal, uint32_t node_size)
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

void EMFVolume::carve_data_to(std::string const& s)
{
  if (!fs::exists(s))
    throw runtime_error(str(format("directory [%s] does not exist") %s));

  m_carve_dir = s;
}

auto EMFVolume::traverse_unused_area(UnusedAreaVisitor visitor) -> bool
{
  auto start = uint32_t((m_journal_offset + m_journal_size) / m_block_size);

  for (uint32_t lba=start; lba<m_header.totalBlocks; ++lba)
  {
    if (block_in_use(uint32_t(lba)))
      continue;

    if (m_decrypted_lbas.find(lba) != m_decrypted_lbas.end())
      continue;

    bool ok = visitor(lba);
    if (!ok)
      return false;
  }

  return true;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
/*
void EMFVolume::carve_unused_area_by_filename(std::string const& name)
{
  if (m_active_file_keys.find(name) == m_active_file_keys.end())
  return;

  auto const& key = m_active_file_keys[name];

  auto start = uint32_t((m_journal_offset + m_journal_size) / m_block_size);
  for (uint32_t lba=start; lba<m_header.totalBlocks; ++lba)
  {
    if (block_in_use(uint32_t(lba)))
      continue;

    if (m_decrypted_lbas.find(lba) != m_decrypted_lbas.end())
    {
      m_decrypted_lbas.insert(lba);
      continue;
    }

    auto bf = read(lba * m_block_size, 16);
    auto b = bf;
    EMFFile ef(this, key, m_protect_version);
    ef.decrypt_buffer(lba, b);

    //    auto it = find_if(sv.begin(), sv.end(), bind(&Signature::match_head, _1, b));
    //    if (it == sv.end())
    //      continue;

    auto path = str(format("%s/%s_%d.bin") % m_carve_dir % name % lba);
    write_file(path, b);
  }
}
     
void EMFVolume::carve_node_slacks_to(std::string const& s)
{
  auto path1 = "/Users/yielding/Desktop/work/a.bin";
  auto path2 = "/Users/yielding/Desktop/work/b.bin";
  
  m_catalog_tree->traverse_leaf_slacks(
    bind(&write_file2, path1, _1));
  
  m_attribute_tree->traverse_leaf_slacks(
    bind(&write_file2, path2, _1));
}
*/

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
