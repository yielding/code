#include "EffaceableLocker.h"

#include <zlib.h>
#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {

    enum LockerValue {
        Dkey = 0x446B6579,
        EMF  = 0x454D4621,
        BAG1 = 0x42414731,
        DONE = 0x444f4e45
    };

    struct Locker
    {
        Locker()
        {
            magic = "";
            data  = "";
        }
        
        Locker(ByteBuffer const& b)
        {
            load(b);
        }
        
        void load(ByteBuffer const& b)
        {
            magic  = b.get_string(2);
            length = b.get_uint2_le();
            tag    = b.slice(4, 8);
            data   = b.get_string(length);
        }
        
        string     magic;
        uint16_t   length;   // unsigned little endian
        ByteBuffer tag;      // union tas: as_integer, as_string
        string     data;
    };
   
    // String: ByteBuffer or std::string
    template <typename String> 
    string xor_strings(String const& s, String const& key)
    {
        string res;

        for (auto i=0; i<s.size(); ++i)
        {
            uint8_t a = s[i];
            uint8_t b = key[i % key.size()];
            res += char(a ^ b);
        }

        return res;
    }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
EffaceableLockers::EffaceableLockers(ByteBuffer const& data)
{
    while (!data.read_all())
    {
        Locker l(data);
        auto tag = l.tag.get_uint4_le() & ~0x80000000;
        // TODO
        // test_byte_buffer에서 검증
        ByteBuffer b;
        b.set_uint4_le(tag);
        auto key = b.reverse_copy().to_s();
        _lockers[key] = l.data;
    }
}

auto EffaceableLockers::get_emf(ByteBuffer const&  k89b) -> ByteBuffer
{
    ByteBuffer res;
    auto pos = _lockers.find("LwVM");
    if (pos != _lockers.end())
        return utility::hex::ByteBuffer();
}

auto EffaceableLockers::get_dkey(ByteBuffer const&  k835) -> ByteBuffer
{
    return utility::hex::ByteBuffer();
}

auto EffaceableLockers::get_locker(ByteBuffer const& tag) -> ByteBuffer
{
    return utility::hex::ByteBuffer();
}

auto EffaceableLockers::to_s() -> string
{
    return "";
}

bool check_effaceable_header(ByteBuffer const& plog)
{
    auto a = plog.slice( 0, 16);
    auto b = plog.slice(16, 32);
    auto z = xor_strings(a, b);
    if (z != "ecaF")
        return false;

    auto plog_generation = plog.offset(0x38).get_uint4_le();
    cout << "Effaceable generation: " << plog_generation << endl;
    
    auto tmpb = plog.slice(0x20, 0x3c);
    auto pbuf = plog.slice(0x40, 0x40 + 960);
    auto crc_to_varify = plog.offset(0x3c).get_uint4_le();

    int64_t crcz     = crc32(0L,   (uint8_t*)z.c_str(), (unsigned)z.length());
    int64_t tmpv     = crc32(crcz, (uint8_t*)tmpb,      (unsigned)tmpb.size());
    int64_t plog_crc = crc32(tmpv, (uint8_t*)pbuf,      (unsigned)pbuf.size());
    return crc_to_varify == plog_crc;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////