#include "emf_volume.h"
#include "key_bag.h"

#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
  //auto path = "/Users/yielding/Desktop/work/data_20120307-1818.dmg";
  auto path = "/Users/yielding/Desktop/work/af9583a0a2b1a8b9.plist";
  KeyBag k = KeyBag::create_with_plist(path);

  /*
  EMFVolume v;
  if (!v.open(path))
    return 1;
  
  v.decrypt_all_files();
  v.carve_data_to("/Users/yielding/Desktop/work/deleted");
  v.undelete();
  */
  
  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
