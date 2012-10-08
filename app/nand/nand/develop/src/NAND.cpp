#include "stdafx.h"

#include "NAND.h"
#include "NANDUtil.h"
#include "NANDImageFlat.h"
#include "DeviceInfo.h"
#include "EffaceableLocker.h"
#include "VFL.h"
#include "VSVFL.h"
#include "YAFTL.h"

#include "AES.h"
#include "SCFG.h"

#include <boost/algorithm/string.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/format.hpp>

#if defined(DEVELOP)
#include <cassert>
#endif

using namespace boost::phoenix::arg_names;
using namespace boost;
using namespace std;

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// REMARK: find appropriate postions for the below anonymous namespace codes
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
namespace
{
    // 
    // TODO REFACTOR => move code position to another place
    //
    nand_chip_info nc_infos[] = {
        // chip_id
        { 0x7294D7EC,  0x1038,  0x80, 0x2000, 0x1B4,  0xC, 0, 8, 1, 0 },
        { 0x72D5DEEC,  0x2070,  0x80, 0x2000, 0x1B4,  0xC, 0, 8, 2, 0 },
        { 0x29D5D7EC,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 2, 2, 0 },
        { 0x2994D5EC,  0x1000,  0x80, 0x1000,  0xDA,    8, 0, 2, 1, 0 },
        { 0xB614D5EC,  0x1000,  0x80, 0x1000,  0x80,    4, 0, 2, 1, 0 },
        { 0xB655D7EC,  0x2000,  0x80, 0x1000,  0x80,    4, 0, 2, 2, 0 },
        { 0xB614D5AD,  0x1000,  0x80, 0x1000,  0x80,    4, 0, 3, 1, 0 },
        { 0x3294E798,  0x1004,  0x80, 0x2000, 0x1C0, 0x10, 0, 1, 1, 0 },
        { 0xBA94D598,  0x1000,  0x80, 0x1000,  0xDA,    8, 0, 1, 1, 0 },
        { 0xBA95D798,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 1, 2, 0 },
        { 0x3294D798,  0x1034,  0x80, 0x2000, 0x178,    8, 0, 1, 1, 0 },
        { 0x3295DE98,  0x2068,  0x80, 0x2000, 0x178,    8, 0, 1, 2, 0 },
        { 0x3295EE98,  0x2008,  0x80, 0x2000, 0x1C0, 0x18, 0, 1, 2, 0 },
        { 0x3E94D789,  0x2000,  0x80, 0x1000,  0xDA, 0x10, 0, 5, 1, 0 },
        { 0x3ED5D789,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 6, 2, 0 },
        { 0x3ED5D72C,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 5, 2, 0 },
        { 0x3E94D72C,  0x2000,  0x80, 0x1000,  0xDA,  0xC, 0, 7, 1, 0 },
        { 0x4604682C,  0x1000, 0x100, 0x1000,  0xE0,  0xC, 0, 7, 1, 0 },
        { 0x3294D745,  0x1000,  0x80, 0x2000, 0x178,    8, 0, 9, 1, 0 },
        { 0x3295DE45,  0x2000,  0x80, 0x2000, 0x178,    8, 0, 9, 2, 0 },
        { 0x32944845,  0x1000,  0x80, 0x2000, 0x1C0,    8, 0, 9, 1, 0 },
        { 0x32956845,  0x2000,  0x80, 0x2000, 0x1C0,    8, 0, 9, 2, 0 }
    };

    auto META_KEY = ByteBuffer::from_hexcode("92a742ab08c969bf006c9412d3cc79a5");

    vector<uint32_t> gen_h2fmi_hash_table()
    {
        vector<uint32_t> res(256, 0);

        uint32_t val = 0x50F4546A;
        for (int i=0; i<256; i++)
        {
            val = 0x19660D * val + 0x3C6EF35F;

            for (int j=1; j<763; j++)
                val = (0x19660D * val) + 0x3C6EF35F;

            res[i] = val;
        }

        return res;
    }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
NAND::NAND(char const* fname, DeviceInfo& dinfo, int64_t ppn)
    : _filename(fname)
    , _dinfo(dinfo)
    , _vfl(nullptr)
{
    _h2fmi_ht = gen_h2fmi_hash_table();

    // TODO REFACTOR
    auto nc_size = sizeof(nc_infos) / sizeof(nc_infos[0]);
    for (int i=0; i<nc_size; i++)
    {
        auto key = nc_infos[i].chip_id;
        _nand_chip_info[key] = nc_infos[i];
    }

    // TODO Check
    // _partition_table = nullptr;
    // _locker = nullptr;

    _ios_version = 0; 
    _has_mbr     = false;
    _metadata_whitening = false;

    char const* models[] = { "M68AP", "N45AP", "N82AP", "N72AP" };
    _encrypted 
        = find(models, models+4, _dinfo.hw_model()) == models+4;

    auto nand = dinfo.nand();
    init_geometry(nand);

    if (starts_with(_filename, "ce"))
    {
        // image = new NANDImageSplitCEs();
    }
    else if (_filename == "remote")
    {
        // image = new NANDImageRemote();
    }
    else
    {
        _image = new NANDImageFlat(_filename.c_str(), nand);
    }
    
    auto   page = read_page(0, 0);
    auto& page0 = page.data;
    
    _nand_only = !page.data.empty() && page0.starts_with("ndrG");
    if (_nand_only)
        _encrypted = true;
    
    vector<string> magics;
    magics.push_back("DEVICEINFOBBT"); // BBT ??????
    ByteBuffer nandsig;
    
    if (!page0.empty() && page0.slice(8, 14).starts_with("Darwin"))
        nandsig = page0;
    else
        magics.push_back("NANDDRIVERSIGN");

    auto sp0 = read_special_pages(0, magics);
    if (!_nand_only)
        cout << "Device does not boto from NAND. (has a NOR)";
    
    char vfl_type = '1';
    
    if (nandsig.empty())
        nandsig = sp0["NANDDRIVERSIGN"];
    
    if (nandsig.empty())
    {
        cout << "NANDDRIVERSIGN not found. assuming metadata whitening :" 
             << _metadata_whitening
             << endl;
    }
    else
    {
        auto nsig  = nandsig.get_uint4_le();
        auto flags = nandsig.get_uint4_le();
        auto epoch = nandsig[0];
        vfl_type   = nandsig[1];
        _metadata_whitening = flags & 0x10000;
#if defined(DEVELOP)
        cout << str(format("NAND signature 0x%x flags 0x%x withening=%d, epoch=%s\n")
                    % nsig % flags % _metadata_whitening % epoch);
#endif
    }
    
    if (!_nand_only)
    {
        _lockers = new EffaceableLockers(_dinfo.lockers());
    }
    else
    {
        auto unit = find_lockers_unit();
        if (!unit.empty())
        {
            _lockers = new EffaceableLockers(unit.slice(0x40, uint32_t(unit.size())));
            if (_dinfo.lockers().empty())
            {
                // TODO: verify the result
                _emf  = _lockers->get_emf(_dinfo.key89B());
                _dkey = _lockers->get_dkey(_dinfo.key835());
            }
        }

        auto device_unique_info = sp0["DEVICEUNIQUEINFO"];
        if (device_unique_info.empty())
        {
            cout << "DEVICEUNIQUEINFO not found\n";
        }
        else
        {
            // TODO verify the result
            auto scfg = parse_scfg(device_unique_info);
            auto srnm = scfg["SrNm"];
            cout << str(format("Found DEVICEUNIQUEINFO, serial number=%s") % srnm.c_str());
            cout.flush();
        }
    }
        
    // TODO verify
    if (vfl_type == '0')
    {
        _vfl = new VFL(*this);
    }
    else if (ppn == -1)
    {
        _vfl = new VSVFL(*this);
        _ftl = new YAFTL(_vfl);
    }
}

NAND::~NAND()
{
    if (_image)
    {
        delete _image; _image = nullptr;
    }

    if (_lockers)
    {
        delete _lockers; _lockers = nullptr;
    }

    if (_vfl)
    {
        delete _vfl; _vfl = nullptr;
    }
}

auto NAND::find_lockers_unit() -> ByteBuffer
{
    ByteBuffer empty;

    if (!_nand_only)
        return empty;

    for (int i=96; i<128; i++)
    {
        for (auto ce=0; ce<_ce_count; ++ce)
        {
            auto page = read_block_page(ce, 1, i, empty);
            if (!page.data.empty() && check_effaceable_header(page.data)) {
#if defined(DEVELOP)
                cout << str(format(
                     "Found effaceable lockers in ce %d block 1page %d\n") % ce % i);
#endif
                return page.data;
            }
        }
    }
    
    return empty;
}

auto NAND::read_page(uint32_t ce_no, uint32_t page_no, ByteBuffer const& key,
                     uint32_t lpn, SpareType st) const -> NANDPage
{
    NANDPage page;

    if (ce_no > _ce_count || page_no > _pages_per_ce)
    {
        // Exception!!!!
        return page;
    }

    if (this->_filename != "remote")
    {
        auto bank = (page_no & ~((1 << _bank_mask) - 1)) >> _bank_mask;
        auto new_page_no = (page_no &  ((1 << _bank_mask) - 1));
        new_page_no = bank * _blocks_per_bank * _pages_per_block + new_page_no;
        page = _image->read_page(ce_no, new_page_no); 
    }
    else
    {
        page = _image->read_page(ce_no, page_no);
    }

    if (page.data.empty())
        return page;           // page should be empty();
    
    if (_metadata_whitening && !page.spare.all_values_are(0x00) && page.spare.size() == 12)
        page.spare = this->unwhiten_metadata(page.spare, page_no);

    // REMARK : other structures except kSpareData don't have lpn field
    //          so, ported code should be like this.
    uint32_t new_lpn = 0;
    if (st == kSpareData)
    {
        SpareData sp(page.spare);
        new_lpn = sp.lpn;
    }

    if (!key.empty() && _encrypted)
    {
        auto new_pn = (lpn != 0xFFFFFFFF) ? new_lpn : page_no;
        page.data   = this->decrypt_page(page.data, key, new_pn);
    }

    return page;
}

auto NAND::read_block_page(uint32_t ce_no, uint32_t block, uint32_t page,
     ByteBuffer const& key, uint32_t lpn, SpareType st) const -> NANDPage
{
    assert(page < _pages_per_block);
    
    uint32_t page_no = block * _pages_per_block + page;
    
    return read_page(ce_no, page_no, key, lpn, st);
}

auto NAND::read_meta_page(uint32_t ce, uint32_t block, uint32_t page, SpareType st) const
    -> NANDPage
{
    return read_block_page(ce, block, page, META_KEY, 0xffffffff, st);
}

auto NAND::read_special_pages(uint32_t ce_no, vector<string>& magics)
    -> map<string, ByteBuffer>
{
    map<string, ByteBuffer> specials;

    if (_nand_only)
        magics.push_back("DEVICEUNIQUEINFO");

    for_each(magics.begin(), magics.end(), 
        [] (string& s) { s.append(16 - s.length(), 0); });

    auto lowest_block = _blocks_per_ce - (_blocks_per_ce / 100);
    for (int block = _blocks_per_ce-1; block > lowest_block; --block)
    {
        if (magics.size() == 0)
            break;

        auto bank_offset = _bank_address_space * (block / _blocks_per_bank);
        for (int page = _pages_per_block; page > -1; --page)
        {
            auto  page_no = (bank_offset + block % _blocks_per_bank) * _pages_per_block + page;
            auto one_page = read_page(ce_no, page_no);
            if (one_page.data.empty())
                continue;

            auto magic = one_page.data.slice(0, 16).to_s();
            auto p0 = find(magics.begin(), magics.end(), magic);
            if (p0 != magics.end())
            {
                _encrypted = false;
                magics.erase(p0);
                auto key = trim_right_copy_if(magic.substr(0, 16), arg1 == 0);
                specials[key] = unpack_spacial_page(one_page.data);
                break;
            }

            one_page.data = decrypt_page(one_page.data, META_KEY, page_no);

            magic = one_page.data.slice(0, 16).to_s();
            auto p1 = find(magics.begin(), magics.end(), magic);
            if (p1 != magics.end())
            {
                _encrypted = true;
                magics.erase(p1);
                auto key = trim_right_copy_if(magic.substr(0, 16), arg1 == 0);
                specials[key] = unpack_spacial_page(one_page.data);
                
                break;
            }
        }
    }

    return specials;
}

auto NAND::unpack_spacial_page(ByteBuffer& data)
    -> ByteBuffer 
{
    auto loc = data.offset(0x34).get_uint4_le();

    return data.slice(0x38, 0x38 + loc);
}

auto NAND::unwhiten_metadata(ByteBuffer& spare_, uint32_t page_no) const
    -> ByteBuffer 
{
    ByteBuffer spare;

    if (spare_.size() == 12)
    {
        for (int i=0; i<3; i++)
        {
            auto v = spare_.get_uint4_le();
            v ^= _h2fmi_ht[(i + page_no) % _h2fmi_ht.size()];
            spare.set_uint4_le(v);
        }
    }
    
    spare.offset(0);
    
    return spare;
}

auto NAND::decrypt_page(ByteBuffer data, ByteBuffer const& key, uint32_t page_no) const
    -> ByteBuffer 
{
    return aes_decrypt_cbc(data, key, iv_for_page(page_no));
}

void NAND::init_geometry(NandInfo const& nand)
{
    _meta_size = nand.meta_per_logical_page;
    if (_meta_size == 0)
        _meta_size = 12;

    auto dumped_page_size = nand.dumped_page_size;
    if (dumped_page_size == 0)
        dumped_page_size = nand.bytes_per_page + _meta_size + 8;

    _dump_size     = int64_t(nand.ce_count)  * nand.blocks_per_ce * nand.pages_per_block * dumped_page_size;
    _total_pages   = uint32_t(nand.ce_count) * nand.blocks_per_ce * nand.pages_per_block;
    auto nand_size = int64_t(_total_pages) * nand.bytes_per_page;

    auto hsize = util::sizeof_fmt(nand_size);
    
    _bfn = nand.boot_from_nand;
    _dumped_page_size = dumped_page_size;

    _page_size = nand.bytes_per_page;
    _bootloader_bytes = std::max<int>(1536, nand.bootloader_bytes);
    _empty_bootloader_page.resize(_bootloader_bytes, 0xff);
    _blank_page.resize(_page_size, 0xff);
    
    _ce_count           = nand.ce_count;
    _blocks_per_ce      = nand.blocks_per_ce;
    _pages_per_block    = nand.pages_per_block;
    _pages_per_ce       = _pages_per_block * _blocks_per_ce;
    _vendor_type        = nand.vendor_type;
    _device_readid      = nand.device_readid;
    _banks_per_ce_vfl   = nand.banks_per_ce;
    _metadata_whitening = nand.metadata_whitening;

    if (_nand_chip_info.find(_device_readid) != _nand_chip_info.end())
        _banks_per_ce_physical = _nand_chip_info[_device_readid].banks_per_ce;
    else
        _banks_per_ce_physical = 1;

    _blocks_per_bank = _blocks_per_ce / _banks_per_ce_physical;
    
    if ((_blocks_per_ce & _blocks_per_ce - 1) == 0)
    {
        _bank_address_space = _blocks_per_bank;
        _total_block_space  = _blocks_per_ce;
    }
    else
    {
        _bank_address_space = util::next_power_of_two(_blocks_per_bank);
        _total_block_space  = (_banks_per_ce_physical - 1) * _bank_address_space + _blocks_per_bank;
    }
    
    _bank_mask = (int) util::log2(_bank_address_space * _pages_per_block);
}

auto NAND::iv_for_page(uint32_t page_no) const -> ByteBuffer
{
    ByteBuffer iv;
    
    for (auto i=0; i<4; i++)
    {
        if (page_no & 1)
            page_no = 0x80000061 ^ (page_no >> 1);
        else
            page_no = page_no >> 1;
        
        iv.set_uint4_le(page_no);
    }

    return iv;
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
