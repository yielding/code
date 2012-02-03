#ifndef EMF_H_CF92LBGB
#define EMF_H_CF92LBGB

#include "ByteBuffer.h"
////////////////////////////////////////////////////////////////////////////////
//
// As of iOS 4, class keys 1 to 4 are used for files, class 5 usage is unknown
//
////////////////////////////////////////////////////////////////////////////////
#define MAX_CLASS_KEYS  5
#define CLASS_DKEY      4

#define CPROTECT_V2_LENGTH 0x38  // 56
#define CP_WRAPPEDKEYSIZE    40  /* 2x4 = 8, 8x8 = 64 */

// http://www.opensource.apple.com/source/xnu/xnu-1699.22.73/bsd/sys/cprotect.h
struct cprotect_xattr_v2
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    xattr_major_version = b.get_uint2_le();
    xattr_minor_version = b.get_uint2_le();
    flags = b.get_uint4_le();
    persistent_class = b.get_uint4_le();
    key_size = b.get_uint4_le();
    for (uint32_t i=0; i<key_size; ++i)
      persistent_key[i] = b.get_uint1();
  }
  
  uint16_t xattr_major_version; // =2
  uint16_t xattr_minor_version; // =0
  uint32_t flags;               // leaks stack dword in one code path (cp_handle_vnop)
  uint32_t persistent_class;
  uint32_t key_size;            //0x28
  uint8_t  persistent_key[0x28];
};

#define CPROTECT_V4_LENGTH    0x4C  //76

struct cprotect_xattr_v4
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    xattr_major_version = b.get_uint2_le();
    xattr_minor_version = b.get_uint2_le();
    flags               = b.get_uint4_le();
    persistent_class    = b.get_uint4_le();
    key_size            = b.get_uint4_le();
    for (int i=0; i<20; i++)  padding[i] = b.get_uint1();
    for (uint32_t i=0; i<key_size; ++i)
      persistent_key[i] = b.get_uint1();
  }
  
  uint16_t xattr_major_version; // =4
  uint16_t xattr_minor_version; // =0
  uint32_t flags;               // 0xc
  uint32_t persistent_class;
  uint32_t key_size;            // 0x28
  uint8_t  padding[20];
  uint8_t  persistent_key[1];
};

struct cp_root_xattr
{
  void read_from(utility::hex::ByteBuffer& b)
  {
    major_version = b.get_uint2_le();
    minor_version = b.get_uint2_le();
    flags         = b.get_uint8_le();
    reserved1     = b.get_uint4_le();
    reserved2     = b.get_uint4_le();
    reserved3     = b.get_uint4_le();
    reserved4     = b.get_uint4_le();
  }
  
  uint16_t major_version; // little endian
  uint16_t minor_version; // little endian
  uint64_t flags;
  uint32_t reserved1;
  uint32_t reserved2;
  uint32_t reserved3;
  uint32_t reserved4;
};

////////////////////////////////////////////////////////////////////////////////
//
// 
//
////////////////////////////////////////////////////////////////////////////////
#endif
