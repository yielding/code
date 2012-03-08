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

  virtual int id() { return -1; }

  int head_count(ByteBuffer& b)
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

  bool match_head(uint8_t* block, uint32_t bs)
  {
    if (bs >= m_head.size())
    {
      if (memcmp(&m_head[0], &block[0], m_head.size()) == 0)
        return true;
    }

    return false;
  }

  int match_head(ByteBuffer& b)
  {
    uint8_t* buffer = b;
    return match_head(buffer, b.size());
  }

  bool match_tail(ByteBuffer& b)
  {
    if (b.size() >= m_tail.size())
    {
      uint8_t* buffer = b;
      auto end = b.size() - m_tail.size();
      for (auto i=0; i<end; i++)
        if (memcmp(&m_tail[0], buffer + i, m_tail.size()) == 0)
          return true;
    }

    return false;
  }

  uint32_t head_size() 
  {
    return m_head.size();
  }

protected:
  std::vector<uint8_t> m_head, m_tail;
};

class JPEGSignature: public Signature
{
public:
  JPEGSignature()
  {
    uint8_t h[3] = { 0xFF, 0xD8, 0xFF };
    uint8_t t[2] = { 0xFF, 0xD9 };

    m_head.assign(h, h+3);
    m_tail.assign(t, t+2);
  }

  virtual int id() { return kJPEG; }
};

class BPlistSignature: public Signature 
{
public:
  BPlistSignature()
  {
    char const* h = "bplist";
    m_head.assign(h, h+6);
  }

  virtual int id() { return kBPLIST; }
};

class SQLiteSignature: public Signature 
{
public:
  SQLiteSignature()
  {
    char const* h = "SQLite";
    m_head.assign(h, h+6);
  }

  virtual int  id() { return kSQLITE; }
};


////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
