#include "key_bag.h"
#include "ByteBuffer.h"
#include "emf.h"
#include "PtreeParser.h"
#include "Base64.h"

#include <iostream>
#include <map>
#include <openssl/sha.h>
#include <boost/format.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>

using namespace std;
using namespace boost;
using phoenix::arg_names::arg1;

using namespace utility::hex;
using namespace utility::parser;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {

typedef pair<string, ByteBuffer> TLVPair;
typedef vector<TLVPair> TLVPairs;

TLVPairs tlv_to_pairs(ByteBuffer& blob)
{
  TLVPairs result;

  uint32_t i = 0;
  while (i + 8 < blob.size())
  {
    auto    tag = blob.slice(i  , i+4);
    auto length = blob.slice(i+4, i+8).get_uint4_be();
    auto  value = blob.slice(i+8, i+8+length);
    i += 8 + length;
    
    auto t = tag.to_str();
    result.push_back(TLVPair(t, value));
  }
  
  return result;
}

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
KeyBag KeyBag::create_with_plist(string const& path)
{
  PTreeParser ptree;
  if (!ptree.init_with_path(path))
    throw std::runtime_error("key file read error");
  
  ptree.in("plist.dict");
//  string emf = ptree.get_string("EMF");
//  if (emf.empty())
//    throw std::runtime_error("emf key does not exist in the key file");

//  auto emf_bytes = utility::Hex::bytes_from_hexcode(emf);
//  m_emfkey.set_encrypt(&emf_bytes[0]);

  auto k835 = ptree.get_string("key835");
  auto data = ptree.get_string("KeyBagKeys");
  if (k835.empty() or data.empty())
    throw std::runtime_error("Main Key does now exist");

  KeyBag kb(data, k835);
  auto passcode_key = ptree.get_string("passcodeKey");
  if (passcode_key.empty())
    throw std::runtime_error("passcodeKey key does not exist in the key file");

  if (!kb.unlock_with_passcode_key(passcode_key))
    throw std::runtime_error("KeyBag unlock failed");

  string dkey = ptree.get_string("DKey");
  if (dkey.empty())
    throw std::runtime_error("Dkey key does not exist in the key file");

  kb.set_dkey(dkey);
  
  return kb;
}

void KeyBag::set_dkey(std::string const& dkey)
{
  auto dkey_bytes = ByteBuffer::from_hexcode(dkey); 
  m_class_keys[4].key.set_decrypt((uint8_t*)dkey_bytes);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
KeyBag::KeyBag()
{
  init_member();
}

KeyBag::KeyBag(string const& data, string const& device_key)
{
  init_member();

  string trimed;
  remove_copy_if(data.begin(), data.end(), back_inserter(trimed),
                 arg1 == '\t' || arg1 == '\n');
  
  auto blob = utility::codec::base64::decode(trimed);

  m_device_key = device_key;
  ByteBuffer blob_(blob);
  m_init_ok = parse_binary_blob(blob_);
}

void KeyBag::init_member()
{
  m_init_ok  = false;
  m_unlocked = false;

  m_vers = 0;
  m_type = 0;
  m_uuid = "";
  m_hmck = "";
  m_wrap = 0;
  m_salt = "";
  m_iter = 0;
  m_device_key = "";
  m_class_keys.clear();
}

bool KeyBag::parse_binary_blob(ByteBuffer& blob)
{         
  auto  klvs = tlv_to_pairs(blob);
  if (klvs.size() == 0)
    return false;
  
  auto   klv = klvs[0].second;      // [0] : "DATA", [1] : "SIGN"
  auto items = tlv_to_pairs(klv);
  if (items.size() < 7)
    return false;

  // 1. find header
  auto it = items.begin();
  for (int count=0; count<7; ++count, ++it)
  {
    auto&  tag = it->first;
    auto& data = it->second;

    if (tag == "VERS") m_vers = data.get_uint4_be();
    else if (tag == "TYPE") m_type = data.get_uint4_be();
    else if (tag == "UUID") m_uuid = data.to_str();
    else if (tag == "HMCK") m_hmck = data.to_str();
    else if (tag == "WRAP") m_wrap = data.get_uint4_be();
    else if (tag == "SALT") m_salt = data.to_str();
    else if (tag == "ITER") m_iter = data.get_uint4_be();
    else 
      return false;
  }

  // 2. find all class keys
  ClassKey key;
  for (; it != items.end(); ++it)
  {
    auto&  tag = it->first;
    auto& data = it->second;
    
    
    if (tag == "UUID") key.uuid = data.to_str();
    else if (tag == "CLAS") key.clas = data.get_uint4_be();
    else if (tag == "WRAP") key.wrap = data.get_uint4_be();
    else if (tag == "KTYP") key.ktyp = data.get_uint4_be();
    else if (tag == "WPKY") key.wpky = data.to_str();
    else if (tag == "PBKY")
      m_pbky = data.to_str();
    else 
      return false;
    
    cout << "[" << tag << "]" << endl;

    if (key.read_all())
    {
      m_class_keys[key.clas] = key;
      key.clear();
    }
  }

  return true;
}

KeyBag::~KeyBag()
{
}

auto KeyBag::unwrap_filekye_for_class(uint32_t pclass, uint8_t* wrapped_key)
  -> pair<bool, HFSKey>
{
  HFSKey filekey;

  /*
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
  */

  return make_pair(true, filekey);
}

bool KeyBag::unlock_with_passcode_key(string const& passcode_key)
{
  auto passcode_key_bytes = ByteBuffer::from_hexcode(passcode_key); 
  HFSKey pkey_; pkey_.set_decrypt((uint8_t*)passcode_key_bytes);
  auto    pkey = pkey_.as_aeskey();

  int const WRAP_PASSCODE = 2;
  int const WRAP_DEVICE   = 1;

  if (m_type != kBackupKeyBag)
    if (m_device_key.empty())
      return false;

  for (auto it=m_class_keys.begin(); it!=m_class_keys.end(); ++it)
  {
    uint8_t fk[32] = { 0 };
    auto wrapped_key = it->second.wpky;
    if (it->second.wrap & WRAP_PASSCODE)
    {
      auto k = AES_unwrap_key(&pkey, NULL, fk, (uint8_t*)wrapped_key.data(), 
                              wrapped_key.length());
      
    }
    
    if (it->second.wrap & WRAP_DEVICE)
    {
      if (m_device_key.empty())
        continue;
      
      auto device_key_bytes = ByteBuffer::from_hexcode(m_device_key);
      HFSKey dkey_; dkey_.set_decrypt((uint8_t*)device_key_bytes, 16); 
      auto   dkey = dkey_.as_aeskey();
      
      uint32_t iv[4] = { 0 };
      AES_cbc_encrypt(fk, fk, 32, &dkey, (uint8_t*)iv, AES_DECRYPT);
      
      vector<uint8_t> v(fk, fk+32);
      cout << ByteBuffer::to_hexcode(v) << endl;
      cout.flush();
    }
  }
  
  return true;
}

auto KeyBag::unwrap_curve25519(uint32_t pclass, uint8_t* wrapped_key)
  -> pair<bool, HFSKey>
{
  return make_pair(false, HFSKey());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
