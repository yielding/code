#include "http_source.h"

#include <boost/asio/ip/tcp.hpp>
#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct HTTPSource::Impl
{
  Impl(string const& url, int timeout=60): m_url(url) , m_timeout(timeout)
  {}

 ~Impl() {}

  auto handshake(string const& url, int timeout) -> bool
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
      cout << "Response returned with status code " << status_code << "\n";
      return false;
    }

    #if 1
    // Process the response headers, which are terminated by a blank line.
    string header;
    while (getline(m_stream, header) && header != "\r")
      cout << header << "\n";

    cout << "\n";
    #endif

    return true;
  }

  auto read(char* s, streamsize n) -> streamsize
  {
    m_stream.read(s, n);

    return m_stream.gcount();
  }

  boost::asio::ip::tcp::iostream m_stream;
  int m_timeout;
  string m_url;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HTTPSource::HTTPSource(string const& url, int timeout)
  : m_impl(new Impl(url, timeout))
{}

HTTPSource::HTTPSource()
  : m_impl(new Impl("", 60))
{}

HTTPSource::~HTTPSource() = default;

auto HTTPSource::handshake(string const& url, int timeout) -> bool
{
  return m_impl->handshake(url, timeout);
}

streamsize HTTPSource::read(char* s, streamsize n)
{ 
  return m_impl->read(s, n);
}

auto HTTPSource::timeout()
{ 
  return m_impl->m_timeout;
}

auto HTTPSource::timeout(int to)
{
  if (to > 0) m_impl->m_timeout = to;
}

auto HTTPSource::url(string const& url)
{
  m_impl->m_url = url;
}

auto HTTPSource::url()
{
  return m_impl->m_url;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
