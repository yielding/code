#include "stdafx.h"

#include <iostream>
#include <string>
#include <cstdlib>
#include <map>

#include "PTreeParser.h"

using namespace std;

struct device_info 
{
    auto load(string const& path) -> bool;

    auto dkey() -> string;
    auto ecid() -> string;
    auto emf()  -> string;
    auto keybag_keys() -> string;
    auto btMac()       -> string;
    auto class_keys(string key) -> string;
    auto data_volume_offset()   -> uint32_t;
    auto data_volume_uuid()     -> string;
    auto hw_model() -> string;
    auto imei()     -> string;
    auto kernel_boot_args() -> string;
    auto key835()        -> string;
    auto key899()        -> string;
    auto key89A()        -> string;
    auto lockers()       -> string;
    auto nand()          -> map<string, string>;
    auto passcode()      -> string;
    auto passcode_key()  -> string;
    auto ramdisk()       -> map<string, string>;

    // compile time, revision
    auto serial_number() -> string;
    auto udid()          -> string;
    auto wifi_mac()      -> string;

private:
    utility::parser::PTreeParser m_pt;
};

bool device_info::load(string const& path)
{
    if (!m_pt.init_with_path(path))
        return false;

    return true;
}

auto device_info::dkey() -> string 
{ 
    return m_pt.get_string("DKey", "plist.dict");
}

auto device_info::ecid() -> string 
{ 
    return m_pt.get_string("ECID", "plist.dict");
}

auto device_info::emf()  -> string 
{ 
    return m_pt.get_string("EMF",  "plist.dict");
}

auto device_info::keybag_keys() -> string 
{
    return m_pt.get_string("KeyBagKeys", "plist.dict");
}

auto device_info::btMac() -> string 
{
    return m_pt.get_string("btMac", "plist.dict");
}

auto device_info::class_keys(string key) -> string
{
  return m_pt.get_string("classKeys", "plist.dict");
}

auto device_info::data_volume_offset() -> uint32_t 
{
    return m_pt.get_int("dataVolumeOffset", "plist.dict");
}

auto device_info::data_volume_uuid() -> string 
{
    return m_pt.get_string("dataVolumeUUID", "plist.dict");
}

auto device_info::hw_model() -> string 
{
    return m_pt.get_string("hwModel", "plist.dict");
}

auto device_info::imei() -> string 
{
    return m_pt.get_string("imei", "plist.dict");
}

auto device_info::kernel_boot_args() -> string 
{
    return m_pt.get_string("kern.bootargs", "plist.dict");
}

auto device_info::key835() -> string 
{
    return "";
}

auto device_info::key899() -> string 
{
    return "";
}

auto device_info::key89A() -> string 
{
    return "";
}

auto device_info::lockers() -> string 
{
    return "";
}

auto device_info::nand() -> map<string, string> 
{
    return map<string, string>();
}

auto device_info::passcode() -> string 
{
    return "";
}

auto device_info::passcode_key() -> string 
{
    return "";
}

auto device_info::ramdisk() -> map<string, string> 
{
    return map<string, string>();
}

auto device_info::serial_number() -> string 
{
    return "";
}

auto device_info::udid() -> string 
{
    return "";
}

auto device_info::wifi_mac() -> string 
{
    return "";
}

int main(int argc, const char *argv[])
{
    string path = "/Volumes/Data.Disk/iphone.nand/4.nand/d0686b9ba2.plist";

    device_info dinfo;
    if (!dinfo.load(path))
    {
        cout << "load file error\n";
        exit(EXIT_FAILURE);
    }

    cout << "DKey       : " << dinfo.dkey() << endl;
    cout << "ECID       : " << dinfo.ecid() << endl;
    cout << "EMF        : " << dinfo.emf()  << endl;
    cout << "KeyBagKeys : " << dinfo.keybag_keys()  << endl;
    cout << "btMac      : " << dinfo.btMac() << endl;
    // TODO
    // cout << "classKeys  : " << dinfo.class_keys() << endl;
    cout << "dataVolumeOffset : " << dinfo.data_volume_offset() << endl;
    cout << "dataVolumeUUID   : " << dinfo.data_volume_uuid() << endl;
    cout << "hwModel          : " << dinfo.hw_model() << endl;
    cout << "IMEI             : " << dinfo.imei() << endl;
    cout << "kern.boot.args   : " << dinfo.kernel_boot_args() << endl;

    cout << "ok\n";

    return 0;
}
