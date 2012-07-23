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
    
    auto keys = dinfo.keybag_keys();

    /*
    cout << "DKey       : " << dinfo.dkey() << endl;
    cout << "ECID       : " << dinfo.ecid() << endl;
    cout << "EMF        : " << dinfo.emf()  << endl;
    cout << "KeyBagKeys : " << dinfo.keybag_keys()  << endl;
    cout << "btMac      : " << dinfo.bt_mac() << endl;
    cout << "dataVolumeOffset : " << dinfo.data_volume_offset() << endl;
    cout << "dataVolumeUUID   : " << dinfo.data_volume_uuid() << endl;
    cout << "hwModel          : " << dinfo.hw_model() << endl;
    cout << "IMEI             : " << dinfo.imei() << endl;
    cout << "kern.boot.args   : " << dinfo.kernel_boot_args() << endl;
    cout << "key835           : " << dinfo.key835()   << endl;
    cout << "key899           : " << dinfo.key899()   << endl;
    cout << "key89A           : " << dinfo.key89A()   << endl;
    cout << "key89B           : " << dinfo.key89B()   << endl;
    cout << "lockers          : " << dinfo.lockers()  << endl;
    cout << "passcode         : " << dinfo.passcode() << endl;
    cout << "passcodeKey      : " << dinfo.passcode_key()  << endl;
    cout << "serialNumber     : " << dinfo.serial_number() << endl;
    cout << "wifiMac          : " << dinfo.wifi_mac()       << endl;
    */

    // TODO
    // cout << "classKeys  : " << dinfo.class_keys() << endl;
    auto n = dinfo.nand();
    
    for (auto it=n.begin(); it!=n.end(); ++it)
        cout << it->first << " : " << trim_copy(it->second) << endl;
    
    return 0;
}
