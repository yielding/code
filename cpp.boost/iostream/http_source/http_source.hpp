#pragma once

#include <boost/iostreams/concepts.hpp>
#include <memory>
#include <string>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HTTPSource: public boost::iostreams::source
{
public:
  HTTPSource();
  explicit HTTPSource(std::string const& url, int timeout=60);
 ~HTTPSource();

  auto handshake(std::string const& url, int timeout=60) -> bool;

public:
  auto read(char* s, std::streamsize n) -> std::streamsize;

  auto timeout(int to);
  auto timeout();

  auto url(std::string const& url);
  auto url();

private:
  struct Impl;
  std::unique_ptr<Impl> m_impl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
