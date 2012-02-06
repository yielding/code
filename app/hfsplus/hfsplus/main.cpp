// #include "hfs_volume.h"
#include "emf_volume.h"

#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  /**/
  EMFVolume v;
  if (!v.open("/Volumes/Lacie/d0686b9ba27e63d6ab97479e089e09b164d1fa84/data_20120107-0058.dmg.org"))
    return 1;
  
  v.decrypt_all_files();
  
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
