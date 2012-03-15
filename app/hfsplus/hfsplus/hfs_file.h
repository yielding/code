#ifndef HFS_FILE_H_A8BYQQMN
#define HFS_FILE_H_A8BYQQMN

#include "hfs.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSVolume;
using utility::hex::ByteBuffer;

class HFSFile
{
public:
    HFSFile(HFSVolume* v, HFSPlusForkData fork, HFSCatalogNodeID fileID, bool deleted=false);
    HFSFile(HFSVolume* v);

    virtual ~HFSFile();

    virtual auto read_block(uint32_t no) -> ByteBuffer;
    virtual auto process_block(int64_t lba, ByteBuffer& buffer, uint32_t bs) 
        -> ByteBuffer&;

    auto read_all_to_buffer(bool trunc=true) -> ByteBuffer;
    void read_all_to_file(std::string const& filename, std::string const& point, bool=true);

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

protected:
    bool             m_partial;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
