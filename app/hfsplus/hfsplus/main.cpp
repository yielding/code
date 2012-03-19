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
    auto path = "/Users/yielding/Desktop/work/data_20120319-1500.dmg";
    // auto path = "/Volumes/Data.Disk/evidence/3.X_3GS_대검25만/IPHONE3GS_Physical_20120201.dmg";
    // auto path = "/Volumes/Data.Disk/evidence/yielding_phone.2011.11.03/3c85f0509771a5e5257742531c9f938abf0ebffa/data_20111103-1523.dmg";
    EMFVolume v;
    if (!v.open(path))
        return 1;

    v.decrypt_all_files();
    v.carve_data_to("/Users/yielding/Desktop/work/deleted");
    v.undelete();

    return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
