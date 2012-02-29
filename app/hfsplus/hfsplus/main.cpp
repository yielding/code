#include "emf_volume.h"

#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  /**/
  EMFVolume v;
  if (!v.open("/Users/yielding/Desktop/work/20120117__IPHONE4_Physical_.dmg"))
    return 1;
  
  v.decrypt_all_files();
  // v.carve_data_to("/Users/yielding/Desktop/deleted");
  // v.carve_unused_area_by_filename("sms.db");
  // v.carve_node_slacks_to("/Users/yielding/Desktop/work/deleted");
  // v.undelete();
  
  /**/
  
  /** /
  HFSVolume v;

  if (!v.open("/Users/yielding/code/app/hfsplus/hfsplus/data/HFSPlus.dmg"))
    return false;

  v.list_folder_contents("/");
  
  if (v.read_file("/README.md", "/Users/yielding/tmp"))
    cout << "ok\n";
  / **/
  
  return 0;
}
