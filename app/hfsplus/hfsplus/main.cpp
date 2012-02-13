// #include "hfs_volume.h"
#include "emf_volume.h"

#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  /**/
  EMFVolume v;
  if (!v.open("/Volumes/Untitled/forensic.data/sjw.80d452e30b40fa0d9fcd46fdfe56a9f8fa55aeb8/data_20111201-0025.dmg"))
    return 1;
  
  v.decrypt_all_files();
  v.undelete("/Users/yielding/Desktop/deleted");
  
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
