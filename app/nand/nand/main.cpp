#include "stdafx.h"

#include <iostream>
#include <cstdlib>
#include <boost/algorithm/string.hpp>
#include "DeviceInfo.h"

using namespace std;
using namespace boost;

int main(int argc, const char *argv[])
{
    string path = "/Users/yielding/code/app/nand/nand/resource/d0686b9ba2.plist";

    DeviceInfo dinfo;
    if (!dinfo.load(path))
    {
        cout << "load file error\n";
        exit(EXIT_FAILURE);
    }
    
    // TODO
    // cout << "classKeys  : " << dinfo.class_keys() << endl;
    auto n = dinfo.nand_partitions();

    for (auto it=n.begin(); it !=n.end(); ++it)
    {
        cout << it->first << ": \n";
        for (auto jt=it->second.begin(); jt!=it->second.end(); ++jt)
            cout << "\t" << jt->first << " : " << jt->second << endl;
    }
    
    return 0;
}
