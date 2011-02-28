#include "http_source.h"

//#include <boost/asio/ip/tcp.hpp>
#include <asio/ip/tcp.hpp>
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
  HTTPSourceImpl(string const& url, string const& data, int timeout=60)
    : m_url(url)
    , m_data(data)
    , m_timeout(timeout)
  {}

  ~HTTPSourceImpl()
  {}

  bool handshake(string const& url, string const& data, int timeout);

  streamsize    read(char* s, streamsize n);
  streamsize    read_all();
  vector<char>& read_buffer();

  bool          bad() { return m_stream.bad(); }

public:
  //boost::asio::ip::tcp::iostream m_stream;
  asio::ip::tcp::iostream m_stream;

  int m_timeout;
  string m_url;
  string m_data;

private:
  vector<char> m_rd_buffer;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HTTPSource::HTTPSource(string const& url, string const& data, int timeout)
  : m_impl(new HTTPSourceImpl(url, data, timeout))
{}

HTTPSource::HTTPSource()
  : m_impl(new HTTPSourceImpl("", "", 60))
{}

bool HTTPSource::handshake()
{
  return m_impl->m_url.empty() 
    ? false 
    : m_impl->handshake(m_impl->m_url, m_impl->m_data, m_impl->m_timeout);
}

bool HTTPSource::handshake(string const& url, string const& data, int timeout)
{
  return m_impl->handshake(url, data, timeout);
}

std::streamsize HTTPSource::read_all()
{
  return m_impl->read_all();
}

std::vector<char>& HTTPSource::read_buffer()
{
  return m_impl->read_buffer();
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

void HTTPSource::data(std::string const& d)
{
  m_impl->m_data = d;
}

std::string HTTPSource::data()
{
  return m_impl->m_data;
}

bool HTTPSource::bad()
{
  return m_impl->bad();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool HTTPSourceImpl::handshake(string const& url, string const& data, int timeout)
{
  // REMARK in ASIO 1.5.1 we can use
  m_stream.expires_from_now(boost::posix_time::seconds(timeout));

  m_stream.connect(url, "http");
  if (!m_stream)
  {
    cout << "Error: " << m_stream.error().message() << std::endl;
    return false;
  }

  string d = data.empty() ? "/" : data;
  m_stream << "GET "   << d << " HTTP/1.0\r\n";
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

  // Process the response headers, which are terminated by a blank line.
  std::string header;
  while (std::getline(m_stream, header) && header != "\r")
    ;
#if 0
    std::cout << header << "\n";
#endif

  return true;
}

streamsize HTTPSourceImpl::read(char* s, streamsize n)
{
  m_stream.read(s, n);
  return m_stream.gcount();
}

streamsize HTTPSourceImpl::read_all()
{
  m_rd_buffer.clear();
  streamsize read = 0;

  while (true)
  {
    int const BUF_SIZE = 4096;
    char buf[BUF_SIZE] = { 0 };

    m_stream.read(buf, BUF_SIZE);
    streamsize n = m_stream.gcount();
    if (n <= 0)
      break;
    
    read += n;
    m_rd_buffer.insert(m_rd_buffer.end(), buf, buf+n);
  }

  return read;
}

vector<char>& HTTPSourceImpl::read_buffer()
{
  return m_rd_buffer;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
