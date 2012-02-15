#ifndef SIGNATURE_H_K3LX5Q19
#define SIGNATURE_H_K3LX5Q19

#include <stdint.h>
#include <vector>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class EMFVolume;

enum { kJPEG = 1, kSQLITE, kPLIST, kBPLIST, kXML };

class Signature
{
public:

public:
  Signature(EMFVolume* vol): m_volume(vol)
  {}

  virtual ~Signature()
  {}

  virtual int id() { return -1; }

  virtual void match(uint32_t lba);

  virtual bool carve_within(uint32_t start, uint32_t end) = 0;

protected:
  EMFVolume* m_volume;
  std::vector<uint8_t> m_head, m_tail;
};

class JPEGSignature: public Signature
{
public:
  JPEGSignature(EMFVolume* vol)
    : Signature(vol)
  {
    uint8_t h[3] = { 0xFF, 0xD8, 0xFF };
    uint8_t t[2] = { 0xFF, 0xD9 };

    m_head.assign(h, h+3);
    m_tail.assign(t, t+2);
  }

  virtual int id() { return kJPEG; }

  virtual bool carve_within(uint32_t start, uint32_t end)
  {
    return false;
  }
};

class BPlistSignature: public Signature 
{
public:
  BPlistSignature(EMFVolume* vol): Signature(vol)
  {
    char const* h = "bplist";
    m_head.assign(h, h+6);
  }

  virtual int id() { return kBPLIST; }

  virtual bool carve_within(uint32_t start, uint32_t end)
  {
    return false;
  }
};

class SQLiteSignature: public Signature 
{
public:
  SQLiteSignature(EMFVolume* vol): Signature(vol)
  {
    char const* h = "SQLite";
    m_head.assign(h, h+6);
  }

  virtual int  id() { return kSQLITE; }

  virtual bool carve_within(uint32_t start, uint32_t end)
  {
    return false;
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
