#ifndef HTTP_SOURCE_H_KCO69NC1
#define HTTP_SOURCE_H_KCO69NC1

#include <stdint.h>
#include <boost/iostreams/concepts.hpp>
#include <boost/asio/ip/tcp.hpp>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace io = boost::iostreams;

class HTTPSource: public io::source
{
public:
  HTTPSource(string const& url, int timeout=60);

  bool init();

  ~HTTPSource();

public:
  std::streamsize read(char_type* s, std::streamsize n);

  int  timeout()       { return m_timeout;           }
  void timouto(int to) { if (to > 0) m_timeout = to; }

private:
  std::string m_url;
  int m_timeout;

  boost::asio::ip::tcp::iostream m_stream;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
