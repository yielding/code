#ifndef DEVICEINFO_H
#define DEVICEINFO_H

#include <stdint.h>
#include "PListParser.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;

struct DeviceInfo 
{
    auto load(string const& path) -> bool;

    auto dkey() -> string;
    auto ecid() -> string;
    auto emf()  -> string;
    auto keybag_keys() -> string;
    auto bt_mac()      -> string;
    auto class_keys()  -> map<string, string>;
    auto data_volume_offset()   -> uint32_t;
    auto data_volume_uuid()     -> string;
    auto hw_model()         -> string;
    auto imei()             -> string;
    auto kernel_boot_args() -> string;
    auto key835()           -> string;
    auto key899()           -> string;
    auto key89A()           -> string;
    auto key89B()           -> string;
    auto lockers()          -> string;
    auto nand()             -> map<string, string>;
    auto nand_partitions()  -> map<string, map<string, string>>;
    auto passcode()         -> string;
    auto passcode_key()     -> string;
    auto ramdisk()          -> map<string, string>;

    // compile time, revision
    auto serial_number() -> string;
    auto udid()          -> string;
    auto wifi_mac()      -> string;
    
private:
    map<string, string> m_class_keys; 
    map<string, map<string, string>> m_partitions; 
    utility::parser::PListParser m_pt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
