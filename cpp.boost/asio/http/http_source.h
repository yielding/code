#ifndef HTTP_SOURCE_H_KCO69NC1
#define HTTP_SOURCE_H_KCO69NC1

#include <boost/iostreams/concepts.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <vector>
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
  HTTPSource(std::string const& url, std::string const& data, int timeout=60);
 ~HTTPSource() {}

  bool  handshake();
  bool  handshake(std::string const& url, std::string const& data, int timeout=60);

public:
  std::streamsize    read(char* s, std::streamsize n);
  std::streamsize    read_all();
  std::vector<char>& read_buffer();

  void        timouto(int to);
  int         timeout();

  void        url(std::string const& url);
  std::string url();

  void        data(std::string const& d);
  std::string data();

private:
  boost::shared_ptr<HTTPSourceImpl> m_impl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
