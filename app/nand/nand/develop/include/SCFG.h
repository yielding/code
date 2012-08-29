#ifndef SCFG_H
#define SCFG_H

#include "ByteBuffer.h"  // vector, string, stdint.h, stdexcept
#include <map>
#include <stdexcept>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using utility::hex::ByteBuffer;
using std::string;

struct SCFGItem
{
    SCFGItem(ByteBuffer const& b) 
    {
        read_from(b);
    }

    void read_from(ByteBuffer const& b);

    string tag;
    string data;
};

struct SCFG
{
    SCFG(ByteBuffer const& b)
    {
        read_from(b);
    }

    void read_from(ByteBuffer const& b);

    string   magic;
    uint32_t length;
    uint32_t unk1;
    uint32_t unk2;
    uint32_t unk3;
    uint32_t unk4;
};

auto parse_scfg(ByteBuffer const& data) 
    -> std::map<string, string>;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
