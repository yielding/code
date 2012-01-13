#ifndef HFS_FILE_H_A8BYQQMN
#define HFS_FILE_H_A8BYQQMN

#include "hfs.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSVolume;

class HFSFile
{
public:
  HFSFile(HFSVolume* v, HFSPlusForkData fork, HFSCatalogNodeID fileID, bool deleted=false);

  auto read_block(uint32_t no) -> utility::hex::ByteBuffer;
  auto read_all_to_buffer(bool trunc) -> utility::hex::ByteBuffer;
  void read_all_to_file(char const* filename, bool trunc);

  uint32_t block_size() { return m_block_size; }

protected:
  HFSVolume*       m_volume;
  uint32_t         m_block_size;
  HFSCatalogNodeID m_fileID;
  uint32_t         m_total_blocks;
  uint64_t         m_logical_size;
  bool             m_deleted;
  HFSPlusExtentDescriptors 
                   m_extents;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
