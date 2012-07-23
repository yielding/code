#include "stdafx.h"
#include "DeviceInfo.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool DeviceInfo::load(string const& path)
{
    if (!m_pt.init_with_path(path))
        return false;

    return true;
}

auto DeviceInfo::dkey() -> string 
{ 
    return m_pt.get_string("DKey", "plist.dict");
}

auto DeviceInfo::ecid() -> string 
{ 
    return m_pt.get_string("ECID", "plist.dict");
}

auto DeviceInfo::emf()  -> string 
{ 
    return m_pt.get_string("EMF",  "plist.dict");
}

auto DeviceInfo::keybag_keys() -> string 
{
    return m_pt.get_string("KeyBagKeys", "plist.dict");
}

auto DeviceInfo::bt_mac() -> string 
{
    return m_pt.get_string("btMac", "plist.dict");
}

auto DeviceInfo::class_keys() -> map<string, string>
{
  if (m_class_keys.empty())
    m_class_keys = m_pt.get_dict("classKeys", "plist.dict");

  return m_class_keys;
}

auto DeviceInfo::data_volume_offset() -> uint32_t 
{
    return m_pt.get_int("dataVolumeOffset", "plist.dict");
}

auto DeviceInfo::data_volume_uuid() -> string 
{
    return m_pt.get_string("dataVolumeUUID", "plist.dict");
}

auto DeviceInfo::hw_model() -> string 
{
    return m_pt.get_string("hwModel", "plist.dict");
}

auto DeviceInfo::imei() -> string 
{
    return m_pt.get_string("imei", "plist.dict");
}

auto DeviceInfo::kernel_boot_args() -> string 
{
    return m_pt.get_string("kern.bootargs", "plist.dict");
}

auto DeviceInfo::key835() -> string 
{
    return m_pt.get_string("key835", "plist.dict");
}

auto DeviceInfo::key899() -> string 
{
    return m_pt.get_string("key899", "plist.dict");
}

auto DeviceInfo::key89A() -> string 
{
    return m_pt.get_string("key89A", "plist.dict");
}

auto DeviceInfo::key89B() -> string 
{
    return m_pt.get_string("key89B", "plist.dict");
}

auto DeviceInfo::lockers() -> string 
{
    return m_pt.get_string("lockers", "plist.dict");
}

auto DeviceInfo::nand() -> map<string, string>
{
    auto leaves = m_pt.get_dict("nand", "plist.dict");
    leaves.erase("partitions");

    return leaves;
}

auto DeviceInfo::nand_partitions() -> map<string, map<string, string>>
{
    if (m_partitions.empty())
    {
        auto res = m_pt.get_dict("partitions", "plist.dict.dict");
        for (auto it=res.begin(); it!=res.end(); ++it)
        {
            auto pname = it->first;
            auto partition = m_pt.get_dict(pname.c_str(), "plist.dict.dict.dict");
            m_partitions[pname] = partition;
        }
    }
    
    return m_partitions;
}

auto DeviceInfo::passcode() -> string 
{
    return m_pt.get_string("passcode", "plist.dict");
}

auto DeviceInfo::passcode_key() -> string 
{
    return m_pt.get_string("passcodeKey", "plist.dict");
}

// TODO
auto DeviceInfo::ramdisk() -> map<string, string> 
{
    return map<string, string>();
}

auto DeviceInfo::serial_number() -> string 
{
    return m_pt.get_string("serialNumber", "plist.dict");
}

auto DeviceInfo::udid() -> string 
{
    return m_pt.get_string("udid", "plist.dict");
}

auto DeviceInfo::wifi_mac() -> string
{
    return m_pt.get_string("wifiMac", "plist.dict");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
/*
int main(int argc, const char *argv[])
{
    string path = "/Users/yielding/code/app/nand/nand/resource/d0686b9ba2.plist";

    DeviceInfo dinfo;
    if (!dinfo.load(path))
    {
        cout << "load file error\n";
        exit(EXIT_FAILURE);
    }

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

    auto np = dinfo.nand_partitions();
    for (auto it=np.begin(); it !=np.end(); ++it)
    {
        cout << it->first << ": \n";
        for (auto jt=it->second.begin(); jt!=it->second.end(); ++jt)
            cout << "\t" << jt->first << " : " << jt->second << endl;
    }
    

    return 0;
}
*/
