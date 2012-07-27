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

struct partition_
{
    string   name;
    uint32_t block_count;
    int64_t  block_offset;
};

struct nand_info
{
    nand_info() { m_empty = true; }
    bool load(utility::parser::PListParser& d);

    bool empty() const { return m_empty; }

    uint16_t pages_per_block;  // 128
    uint32_t bootloader_bytes; // 1536
    uint16_t ce_count;         // 4
    uint32_t blocks_per_ce;    // 4100 
    uint32_t bytes_per_page;   // 8192
    uint16_t spare_byte_count; // 448
    uint16_t banks_per_ce;     // 1
    uint16_t bbt_format;      // 1
    string   boot_from_nand;   // AQAAA==
    uint64_t device_readid;    // 848619416
    int64_t  dumped_page_size; 
    uint16_t meta_per_logical_page; // 12
    string   metadata_whitening;    // AQAAA== 
    string   name;
    vector<partition_> partitions;
    bool     ppn_device;
    string   use_4k_aes_chain;
    uint32_t valid_meta_per_logical_page;
    uint32_t vendor_type;

private:
    bool m_empty;
};

struct DeviceInfo 
{
    auto load(string const& path) -> bool;

    auto class_keys()  -> map<string, string>;
    auto nand()        -> nand_info;

    auto dkey() const -> string;
    auto ecid() const -> string;
    auto emf()  const -> string;
    auto keybag_keys() const -> string;
    auto bt_mac()      const -> string;
    auto data_volume_offset() const -> uint32_t;
    auto data_volume_uuid()   const -> string;
    auto hw_model()           const -> string;
    auto imei()               const -> string;
    auto kernel_boot_args()   const -> string;
    auto key835()             const -> string;
    auto key899()             const -> string;
    auto key89A()             const -> string;
    auto key89B()             const -> string;
    auto lockers()            const -> string;
    auto passcode()           const -> string;
    auto passcode_key()       const -> string;
    auto ramdisk()            const -> map<string, string>;

    // compile time, revision
    auto serial_number() const -> string;
    auto udid()          const -> string;
    auto wifi_mac()      const -> string;
    
private:
    nand_info m_nand;
    map<string, string> m_class_keys; 
    utility::parser::PListParser m_pt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
