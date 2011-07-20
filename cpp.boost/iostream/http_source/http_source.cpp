#include "http_source.h"

#include <boost/asio/ip/tcp.hpp>
#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HTTPSourceImpl
{
public:
  HTTPSourceImpl(string const& url, int timeout=60)
    : m_url(url)
    , m_timeout(timeout)
  {}

  ~HTTPSourceImpl()
  {}

  bool handshake(string const& url, int timeout);

  streamsize read(char* s, streamsize n);

public:
  boost::asio::ip::tcp::iostream m_stream;
  int m_timeout;
  std::string m_url;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HTTPSource::HTTPSource(string const& url, int timeout)
  : m_impl(new HTTPSourceImpl(url, timeout))
{}

HTTPSource::HTTPSource()
  : m_impl(new HTTPSourceImpl("", 60))
{}

bool HTTPSource::handshake()
{
  return m_impl->m_url.empty() 
    ? false 
    : m_impl->handshake(m_impl->m_url, m_impl->m_timeout);
}

bool HTTPSource::handshake(std::string const& url, int timeout)
{
  return m_impl->handshake(url, timeout);
}

std::streamsize HTTPSource::read(char* s, std::streamsize n)
{ 
  return m_impl->read(s, n);
}

int HTTPSource::timeout() 
{ 
  return m_impl->m_timeout;
}

void HTTPSource::timouto(int to)
{
  if (to > 0)
    m_impl->m_timeout = to;
}

void HTTPSource::url(std::string const& url)
{
  m_impl->m_url = url;
}

std::string HTTPSource::url()
{
  return m_impl->m_url;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool HTTPSourceImpl::handshake(string const& url, int timeout)
{
  // REMARK ASIO 1.5.1
  // m_stream.expires_from_now(boost::posix_time::seconds(timeout));

  m_stream.connect(url, "http");
  if (!m_stream)
  {
    cout << "Unable to connect\n ";
    return false;
  }

  string data = "/LICENSE_1_0.txt";
  m_stream << "GET "   << data << " HTTP/1.0\r\n";
  m_stream << "Host: " << url << "\r\n";
  m_stream << "Accept: */*\r\n";
  m_stream << "Connection: close\r\n\r\n";

  string http_version;      m_stream >> http_version;
  unsigned int status_code; m_stream >> status_code;
  string status_message;    getline(m_stream, status_message);
  if (!m_stream || http_version.substr(0, 5) != "HTTP/")
  {
    cout << status_message;
    return false;
  }

  if (status_code != 200)
  {
    std::cout << "Response returned with status code " << status_code << "\n";
    return false;
  }

#if 1
  // Process the response headers, which are terminated by a blank line.
  std::string header;
  while (std::getline(m_stream, header) && header != "\r")
    std::cout << header << "\n";

  std::cout << "\n";
#endif

  return true;
}

streamsize HTTPSourceImpl::read(char* s, streamsize n)
{
  m_stream.read(s, n);
  return m_stream.gcount();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
