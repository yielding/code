#include "stdafx.h"

#include <iostream>
#include <cstdlib>
#include <boost/algorithm/string.hpp>
#include "DeviceInfo.h"
#include "NANDImageFlat.h"
#include "NAND.h"

using namespace std;
using namespace boost;

int main(int argc, const char *argv[])
{
    string path  = "/Volumes/Data.Disk/iphone.nand/4.nand/iphone4_d0686b9ba2.bin";
    string plist = "/Volumes/Data.Disk/iphone.nand/4.nand/d0686b9ba2.plist";
    DeviceInfo dinfo;
    if (!dinfo.load(plist))
    {
        cout << "load file error\n";
        exit(EXIT_FAILURE);
    }
    
    auto geometry = dinfo.nand();
    
    NAND n(path.c_str(), dinfo);
    
    return 0;
}
