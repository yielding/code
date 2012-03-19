#ifndef SIGNATURE_H_K3LX5Q19
#define SIGNATURE_H_K3LX5Q19

#include "ByteBuffer.h"

#include <string.h>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using utility::hex::ByteBuffer;

enum { 
    kJPEG   = 0, 
    kSQLITE = 1, 
    kBPLIST = 2,
    kPLIST  = 3, 
    kXML    = 4
};

class Signature
{
public:
    Signature() {}

    virtual ~Signature() {}

public:
    int  head_count(ByteBuffer& b);
    bool match_head(uint8_t* block, uint32_t bs);
    bool match_head(ByteBuffer& b);
    bool match_tail(ByteBuffer& b);

    uint32_t head_size();

    virtual int id() { return -1; }

protected:
    std::vector<uint8_t> m_head, m_tail;
};

class JPEGSignature: public Signature
{
public:
    JPEGSignature();

    virtual int id() { return kJPEG; }
};

class BPlistSignature: public Signature 
{
public:
    BPlistSignature();

    virtual int id() { return kBPLIST; }
};

class SQLiteSignature: public Signature 
{
public:
    SQLiteSignature();

    virtual int  id() { return kSQLITE; }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
