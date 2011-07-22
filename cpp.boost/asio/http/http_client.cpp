#include "http_client.h"

#include <boost/asio/ip/tcp.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/scoped_array.hpp>
#include <fstream>
#include <sstream>
#include <cassert>
#include <stdint.h>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;

class HTTPClientImpl
{
public:
  HTTPClientImpl(char const* host, char const* port="8080", int timeout=5);
  ~HTTPClientImpl();

  pair<bool, string> get(char const* query);

  string get_raw();

  uint32_t last_status();

private:
  bool handshake(char const* query);
  bool parse_header();

private:
  boost::asio::ip::tcp::iostream m_stream;
  string m_host;
  string m_port;

  int  m_timeout;
  bool m_chunk_based;
  uint32_t m_last_status;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HTTPClientImpl::HTTPClientImpl(char const* host, char const* port, int timeout)
  : m_host(host)
  , m_port(port)
  , m_timeout(timeout)
{
  m_chunk_based = false;
  m_last_status = 0;
}

HTTPClientImpl:: ~HTTPClientImpl()
{
  m_stream.close();
}

// To TEST
// 1. 연속 call 이 가능한지 connect, close
//
pair<bool, string> HTTPClientImpl::get(char const* query)
{
  string data;
  bool res = false;

  if (!handshake(query) || !parse_header())
    return make_pair(res, data);

  if (m_chunk_based)
  {
    uint32_t size = 0;

    while (true)
    {
      string line; getline(m_stream, line);
      size_t to_read = strtol(line.c_str(), 0, 16);
      if (to_read == 0)
        break;

      boost::scoped_array<char> buf(new char[to_read+2]); // 2 for additional 0x0d0a
      m_stream.read(buf.get(), to_read+2);                // consume bytes
      assert(to_read+2 == m_stream.gcount());
      data.append(buf.get(), to_read);

      size += to_read;
    }
#if defined(DEBUG)
    cout << "size: " << size << endl;
#endif
    res = true;
  }
  else
  {
    assert(false);
  }

  return make_pair(res, data);
}

string HTTPClientImpl::get_raw()
{
  char big_buffer[1024*200] = { 0 };
  m_stream.read(big_buffer, 1024 * 200);
  return string(big_buffer, m_stream.gcount());
}

bool HTTPClientImpl::handshake(char const* query)
{
  try
  {
    m_stream.expires_from_now(boost::posix_time::seconds(m_timeout));
    m_stream.connect(m_host, m_port);

#if defined(DEBUG)
    cout << "timeout: " << m_timeout << endl
         << "host   : " << m_host << endl
         << "port   : " << m_port << endl
         << "query  : " << query << endl;
#endif

    if (!m_stream)
    {
      cout << "Unable to connect: " << m_stream.error().message() << "\n";
      return false;
    }

    m_stream << "GET "   << query << " HTTP/1.1\r\n";
    m_stream << "Host: " << m_host << "\r\n";
    m_stream << "Accept: */*\r\n";
    m_stream << "Connection: close\r\n\r\n";

    // check if response is OK.
    string http_version;  m_stream >> http_version;
                          m_stream >> m_last_status;

    string status_message;     
    getline(m_stream, status_message);

    if (!m_stream || http_version.substr(0, 5) != "HTTP/")
    {
      cout << "Invalid response\n";
      return false;
    }

    if (m_last_status != 200)
    {
      cout << "Response returned with status code " << m_last_status << "\n";
      return false;
    }
  }
  catch(exception& e)
  {
    cout << "Exception: " << e.what() << endl;
    return false;
  }

  return true;
}

bool HTTPClientImpl::parse_header()
{
  string header;
  while (getline(m_stream, header) && header != "\r")
  {
    if (boost::find_first(header, "chunked"))
      m_chunk_based = true;
  }

  return true;
}

uint32_t HTTPClientImpl::last_status()
{
  return m_last_status;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HTTPClient::HTTPClient(char const* host, char const* port, int timeout)
{
  m_spHttp.reset(new HTTPClientImpl(host, port, timeout));
}

pair<bool, string> HTTPClient::get(char const* query)
{
  return m_spHttp->get(query);
}

string HTTPClient::get_raw()
{
  return m_spHttp->get_raw();
}

uint32_t HTTPClient::last_status()
{
  return m_spHttp->last_status();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
