#include "key_bag.h"
#include "ByteBuffer.h"
#include "emf.h"

#include <boost/format.hpp>
#include <map>
#include <iostream>

using namespace std;
using namespace boost;
using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  map<string, ByteBuffer> tlv_to_map(ByteBuffer& blob)
  {
    map<string, ByteBuffer> result;
    
    uint32_t i = 0;
    while (i + 8 < blob.size())
    {
      auto    tag = blob.slice(i  , i+4);
      auto length = blob.slice(i+4, i+8).get_uint4_be();
      auto  value = blob.slice(i+8, i+8+length);
      i += 8 + length;
      
      auto t = tag.to_str();
      result[t] = value;
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
  KeyBag kb;
  
  return kb;
}

KeyBag KeyBag::create_with_data_sign_blob(ByteBuffer& blob, string const& device_key)
{
  auto klv = tlv_to_map(blob);
  
  KeyBag kb(klv["DATA"]);
  kb.device_key(device_key);
  
  
  return kb;
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

KeyBag::KeyBag(ByteBuffer data)
{
  init_member();
  parse_binary_blob(data);
}

void KeyBag::init_member()
{
  m_type = "";
  m_uuid = "";
  m_wrap = "";
  m_device_key = "";
  m_unlocked = false;
  m_attrs.clear();
  m_class_keys.clear();
}

void KeyBag::parse_binary_blob(ByteBuffer& data)
{         
  auto tlvs = tlv_to_map(data);
  for (auto it = tlvs.begin(); it != tlvs.end(); ++it)
  {
    auto  tag = it->first;
    auto data = it->second;
  }
}

KeyBag::~KeyBag()
{
}

auto KeyBag::unwrap_filekye_for_class(uint32_t pclass, uint8_t* wrapped_key)
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

auto KeyBag::unwrap_curve25519(uint32_t pclass, uint8_t* wrapped_key)
  -> pair<bool, HFSKey>
{
  return make_pair(false, HFSKey());
}
