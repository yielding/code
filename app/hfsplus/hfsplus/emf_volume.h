#ifndef EMF_VOLUME_H_X03CV5QQ
#define EMF_VOLUME_H_X03CV5QQ

#include "hfs_volume.h"
#include "emf.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class EMFVolume: public HFSVolume
{
public:
  EMFVolume(int64_t offset=0);
  virtual ~EMFVolume();

  virtual auto open(std::string const& filename) -> bool;

public:
  /*
  void get_file_key_for_cprotect(cp);
  
  void read_file(std::string const& path);
  
  void iv_for_lba(uint32_t lba);
  
  void decrypt_all_files();
  
  void decrypt_file();
   */

private:
  cp_root_xattr m_cp_root;
  HFSCatalogNodeID m_metadata_dir;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
