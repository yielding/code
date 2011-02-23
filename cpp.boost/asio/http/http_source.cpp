#include "stdafx.h"
#include "http_source.h"

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HTTPSource::HTTPSource(string const& url, int timeout)
  : m_url(url)
  , m_timeout(timeout)
{
}

bool HTTPSource::init()
{
  // m_stream.expires_from_now(boost::posix_time::seconds(timeout));

  m_stream.connect(m_url, "http");
  if (!m_stream)
  {
    cout << "Unable to connect: " << s.error().message() << "\n";
    return false;
  }

  string data = "LICENSE_1_0.txt";
  m_stream << "GET "   << data << " HTTP/1.0\r\n";
  m_stream << "Host: " << m_url << "\r\n";
  m_stream << "Accept: */*\r\n";
  m_stream << "Connection: close\r\n\r\n";

  string http_version;      m_stream >> http_version;
  unsigned int status_code; m_stream >> status_code;
  string status_message;    getline(m_stream, status_message);
  if (!m_stream || http_version.substr(0, 5) != "HTTP/")
  {
    std::cout << "Invalid response\n";
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

HTTPSource::~HTTPSource()
{}

streamsize HTTPSource::read(char_type* s, std::streamsize n)
{
  m_stream.read(s, n);
  return m_stream.gcount();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
