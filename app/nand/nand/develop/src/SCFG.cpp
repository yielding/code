#include "SCFG.h"

#include <algorithm>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void SCFGItem::read_from(ByteBuffer const& b)
{
    tag = b.get_string(4);
    tag = b.get_string(16);
}


void SCFG::read_from(ByteBuffer const& b)
{
    magic  = b.get_string(4);
    if (magic != "gfCS")
        throw std::runtime_error("wrong signature for SCFG structure");

    length = b.get_uint4_le();
    unk1   = b.get_uint4_le();
    unk2   = b.get_uint4_le();
    unk3   = b.get_uint4_le();
    unk4   = b.get_uint4_le();
}

auto parse_scfg(ByteBuffer const& data) -> map<string, string>
{
    std::map<string, string> result;

    SCFG scfg(data);

    if (scfg.length < 0x18)
        throw std::runtime_error("invalid SCFG length");

    while (data.has_remaining())
    {
        SCFGItem item(data);
        if (item.tag != "\xFF\xFF\xFF\xFF")
        {
            string key = item.tag;
            std::reverse(key.begin(), key.end());
            result[key] = item.data;
        }
    }

    return result;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
