#include "stdafx.h"
#include "DeviceInfo.h"

#include <boost/lexical_cast.hpp>

using namespace boost;
using namespace utility::parser;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool nand_info::load(PListParser& d)
{
    auto nand = d.get_dict("nand", "plist.dict");

    pages_per_block  = lexical_cast<uint16_t>(nand["#block-pages"]);
    bootloader_bytes = lexical_cast<uint32_t>(nand["#bootloader-bytes"]);
    ce_count         = lexical_cast<uint16_t>(nand["#ce"]);
    blocks_per_ce    = lexical_cast<uint32_t>(nand["#ce-blocks"]);
    bytes_per_page   = lexical_cast<uint32_t>(nand["#page-bytes"]);
    spare_byte_count = lexical_cast<uint16_t>(nand["#spare-bytes"]);
    banks_per_ce     = lexical_cast<uint16_t>(nand["banks-per-ce"]);
    bbt_format       = lexical_cast<uint16_t>(nand["bbt-format"]);
    boot_from_nand   = nand["boot-from-nand"];
    device_readid    = lexical_cast<uint64_t>(nand["device-readid"]);
    meta_per_logical_page = lexical_cast<uint16_t>(nand["meta-per-logical-page"]);
    
    auto ds = nand["dumpedPageSize"];
    dumped_page_size = ds.empty() ? bytes_per_page + meta_per_logical_page + 8
                                  : lexical_cast<uint64_t>(ds);
    
    metadata_whitening = nand["metadata-whitening"];
    name       = nand["name"];
    ppn_device = nand["ppn-device"] == "true";
    use_4k_aes_chain = nand["use-4k-aes-chain"];
    valid_meta_per_logical_page = lexical_cast<uint32_t>(nand["valid-meta-per-logical-page"]);
    vendor_type = lexical_cast<uint32_t>(nand["vendor-type"]);

    partition_ p;
    auto plist = d.get_dict("partitions", "plist.dict.dict");
    for (auto it=plist.begin(); it!=plist.end(); ++it)
    {
        p.name = it->first;
        auto items = d.get_dict(p.name.c_str(), "plist.dict.dict.dict");
        p.block_count  = lexical_cast<uint32_t>(items["Block Count"]);
        p.block_offset = lexical_cast<int64_t>(items["Block Offset"]);
        partitions.push_back(p);
    }
    
    return true;
}

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

auto DeviceInfo::dkey() const -> string 
{ 
    return m_pt.get_string("DKey", "plist.dict");
}

auto DeviceInfo::ecid() const -> string 
{ 
    return m_pt.get_string("ECID", "plist.dict");
}

auto DeviceInfo::emf() const -> string 
{ 
    return m_pt.get_string("EMF",  "plist.dict");
}

auto DeviceInfo::keybag_keys() const -> string 
{
    return m_pt.get_string("KeyBagKeys", "plist.dict");
}

auto DeviceInfo::bt_mac() const -> string 
{
    return m_pt.get_string("btMac", "plist.dict");
}

auto DeviceInfo::class_keys() -> map<string, string>
{
  if (m_class_keys.empty())
    m_class_keys = m_pt.get_dict("classKeys", "plist.dict");

  return m_class_keys;
}

auto DeviceInfo::data_volume_offset() const -> uint32_t 
{
    return m_pt.get_int("dataVolumeOffset", "plist.dict");
}

auto DeviceInfo::data_volume_uuid() const -> string 
{
    return m_pt.get_string("dataVolumeUUID", "plist.dict");
}

auto DeviceInfo::hw_model() const -> string 
{
    return m_pt.get_string("hwModel", "plist.dict");
}

auto DeviceInfo::imei() const -> string 
{
    return m_pt.get_string("imei", "plist.dict");
}

auto DeviceInfo::kernel_boot_args() const -> string 
{
    return m_pt.get_string("kern.bootargs", "plist.dict");
}

auto DeviceInfo::key835() const -> string 
{
    return m_pt.get_string("key835", "plist.dict");
}

auto DeviceInfo::key899() const -> string 
{
    return m_pt.get_string("key899", "plist.dict");
}

auto DeviceInfo::key89A() const -> string 
{
    return m_pt.get_string("key89A", "plist.dict");
}

auto DeviceInfo::key89B() const -> string 
{
    return m_pt.get_string("key89B", "plist.dict");
}

auto DeviceInfo::lockers() const -> string 
{
    return m_pt.get_string("lockers", "plist.dict");
}

auto DeviceInfo::nand() -> nand_info
{
    if (m_nand.empty())
        m_nand.load(m_pt);

    return m_nand;
}

auto DeviceInfo::passcode() const -> string 
{
    return m_pt.get_string("passcode", "plist.dict");
}

auto DeviceInfo::passcode_key() const -> string 
{
    return m_pt.get_string("passcodeKey", "plist.dict");
}

// TODO
auto DeviceInfo::ramdisk() const -> map<string, string> 
{
    return map<string, string>();
}

auto DeviceInfo::serial_number() const -> string 
{
    return m_pt.get_string("serialNumber", "plist.dict");
}

auto DeviceInfo::udid() const -> string 
{
    return m_pt.get_string("udid", "plist.dict");
}

auto DeviceInfo::wifi_mac() const -> string
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
