#ifndef PROTOCOL_H_ZPAYBNGQ
#define PROTOCOL_H_ZPAYBNGQ

#include <iostream>
//#include "clone.h"

using namespace std;

class protocol
{
public:
  protocol() {}
  protocol* clone() { return do_clone(); }

  virtual char const* name() { return "protocol"; }

  virtual ~protocol () {}

private:
  virtual protocol* do_clone() = 0;
};

class rfid_protocol: public protocol
{
public:
  rfid_protocol() { }

  rfid_protocol(rfid_protocol const& rhs)
  {
    cout << "rfid_protocol is cloned\n";
  }
  
  virtual ~rfid_protocol () { }
  
  char const* name()
  {
    return "rfid_protocol";
  }

private:
  virtual rfid_protocol* do_clone() { return new rfid_protocol(*this); }
};

class rtls_protocol: public protocol
{
public:
  rtls_protocol() { }
  
  rtls_protocol(rtls_protocol const& rhs)
  {
    cout << "rtls_protocol is cloned\n";
  }
  
  virtual ~rtls_protocol () { }
  
  char const* name()
  {
    return "rtls_protocol";
  }

private:
  virtual rtls_protocol* do_clone() { return new rtls_protocol(*this); }
};

#endif /* end of include guard: PROTOCOL_H_ZPAYBNGQ */
