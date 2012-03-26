#include "signature.h"
#include "emf_volume.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int Signature::head_count(ByteBuffer& b)
{
    if (b.size() < m_head.size())
        return 0;

    int count = 0;
    uint32_t end =  b.size() - m_head.size();
    uint8_t* block = b;
    for (uint32_t i=0; i<end; i++)
        if (memcmp(&m_head[0], (uint8_t*)&block[i], m_head.size()) == 0)
            count++;

    return count;
}

bool Signature::match_head(uint8_t* block, uint32_t bs)
{
    if (bs >= m_head.size())
    {
        if (memcmp(&m_head[0], &block[0], m_head.size()) == 0)
            return true;
    }

    return false;
}

bool Signature::match_head(ByteBuffer& b)
{
    uint8_t* buffer = b;
    return match_head(buffer, b.size());
}

bool Signature::match_tail(ByteBuffer& b)
{
    if (b.size() >= m_tail.size())
    {
        uint8_t* buffer = b;
        auto end = b.size() - m_tail.size();
        for (size_t i=0; i<end; i++)
            if (memcmp(&m_tail[0], buffer + i, m_tail.size()) == 0)
                return true;
    }

    return false;
}

uint32_t Signature::head_size() 
{
    return m_head.size();
}

JPEGSignature::JPEGSignature()
{
    uint8_t h[3] = { 0xFF, 0xD8, 0xFF };
    uint8_t t[2] = { 0xFF, 0xD9 };

    m_head.assign(h, h+3);
    m_tail.assign(t, t+2);
}

BPlistSignature::BPlistSignature()
{
    char const* h = "bplist";
    m_head.assign(h, h+6);
}

SQLiteSignature::SQLiteSignature()
{
    char const* h = "SQLite";
    m_head.assign(h, h+6);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
