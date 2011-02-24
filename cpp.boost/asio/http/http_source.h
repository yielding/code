#ifndef HTTP_SOURCE_H_KCO69NC1
#define HTTP_SOURCE_H_KCO69NC1

#include <boost/iostreams/concepts.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HTTPSourceImpl;

class HTTPSource: public boost::iostreams::source
{
public:
  HTTPSource();
  HTTPSource(std::string const& url, int timeout=60);
 ~HTTPSource() {}

  bool  handshake();
  bool  handshake(std::string const& url, int timeout=60);

public:
  std::streamsize 
        read(char* s, std::streamsize n);

  void  timouto(int to);
  int   timeout();

  void  url(std::string const& url);
  std::string
        url();

private:
  boost::shared_ptr<HTTPSourceImpl> m_impl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
