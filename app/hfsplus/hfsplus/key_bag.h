#ifndef KEY_BAG_H_2PK4YOQP
#define KEY_BAG_H_2PK4YOQP

#include "ByteBuffer.h"
#include "hfs_key.h"

#include <string>
#include <map>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using utility::hex::ByteBuffer;

class KeyBag
{
public: // factory functions
  static KeyBag create_with_plist(std::string const&);
  static KeyBag create_with_data_sign_blob(ByteBuffer&, std::string const&);

public:
  KeyBag();
  KeyBag(ByteBuffer);
  ~KeyBag();

public:
  void device_key(std::string const& dk) { m_device_key = dk; }
  
  auto unwrap_filekye_for_class(uint32_t pclass, uint8_t* wrapped_key)
    -> std::pair<bool, HFSKey>;

private:
  void init_member();
  
private:
  auto unwrap_curve25519(uint32_t pclass, uint8_t* wrapped_key)
    -> std::pair<bool, HFSKey>;

  void parse_binary_blob(ByteBuffer& data);

private:
  std::vector<HFSKey> m_class_keys;
  uint32_t m_class_keys_bitset;
  
  std::map<std::string, ByteBuffer> m_attrs;
  
  std::string m_device_key;
  std::string m_type;
  std::string m_uuid;
  std::string m_wrap;
  bool m_unlocked;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif /* end of include guard: KEY_BAG_H_2PK4YOQP */
