#ifndef EMF_FILE_H_2N1X36JD
#define EMF_FILE_H_2N1X36JD

#include "hfs_file.h"
#include "emf_volume.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class EMFFile: public HFSFile
{
public:
    EMFFile(HFSVolume* volume, HFSPlusForkData const& fork_data
        , HFSCatalogNodeID fileID , HFSKey const& filekey
        , bool deleted=false);

    EMFFile(HFSVolume* volume , HFSKey const& filekey , uint16_t protection_version);

    virtual ~EMFFile();

public:
    void decrypt_file();

    auto decrypt_buffer(uint32_t lba, ByteBuffer& buffer)
        -> ByteBuffer&;

    static bool decrypted_correctly(ByteBuffer& buffer);

public:
    uint32_t start_lba();

private:
    void init_ivkey();

private:
    virtual auto process_block(int64_t lba, ByteBuffer& buffer, uint32_t bs=0)
        -> ByteBuffer&;

private:
    int64_t  m_decrypt_offset;
    uint16_t m_protection_version;
    HFSKey   m_file_key, m_ivkey;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
