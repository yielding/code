#include "hfs_volume.h"

#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  HFSVolume v;

  if (!v.open("/Users/yielding/tmp/hfsplus/hfsplus/data/HFSPlus.dmg"))
    return false;

  v.list_folder_contents("/");
  
  cout << "open ok\n";

  return 0;
}
