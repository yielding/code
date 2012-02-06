#include "emf_file.h"
#include "emf_volume.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EMFFile::EMFFile(HFSVolume* volume, HFSPlusForkData const& fork, HFSCatalogNodeID fileID
  , AES_KEY const& filekey, bool deleted)
  : HFSFile(volume,fork, fileID, deleted)
{
  m_decrypt_offset = 0;
  if (volume->protection_version() == 4)
  {    
    /*
    uint8_t fk[32] = { 0 };
    boost::crypto::sha1 sha_;
    sha_.reset();
    sha_.input(fk, fk+32);                   
    sha_.to_buffer(fk);                     // set the sha1 hashed result into the 'fk'

    AES_set_encrypt_key(fk, 16*8, &m_ivkey);  // takes only the first 16 bytes
    */
    m_ivkey;
  }
}

EMFFile::~EMFFile()
{

}

void EMFFile::decrypt_file()
{

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
