#include "md5.h"

#include "mbedtls/md5.h"

#include <iostream>

using namespace std; 

namespace mbedtls {
////////////////////////////////////////////////////////////////////////////////
//
// 
//
////////////////////////////////////////////////////////////////////////////////
struct md5::impl
{
  impl() 
  { 
    ::mbedtls_md5_init(&ctx); 
    ::mbedtls_md5_starts(&ctx);
  }

  ~impl() { ::mbedtls_md5_free(&ctx); }

  void update(string const& buf)
  {
    mbedtls_md5_update(&ctx, (uint8_t*)buf.data(), buf.size());
  }

  void update(uint8_t* buf, size_t len)
  {
    mbedtls_md5_update(&ctx, (uint8_t*)buf, len);
  }

  auto finish() -> string
  {
    string result(16, 0);
    mbedtls_md5_finish(&ctx, (uint8_t*)result.data());

    return result;
  }

private:
  mbedtls_md5_context ctx;
};

////////////////////////////////////////////////////////////////////////////////
//
// 
//
////////////////////////////////////////////////////////////////////////////////
md5::md5()
{
  m_impl.reset(new impl());
}

md5::~md5() { }

auto md5::reset() -> void
{
  m_impl.reset(new impl());
}

auto md5::update(string const& buf) -> void
{
  return m_impl->update(buf);
}

auto md5::update(uint8_t* buf, size_t length) -> void
{
  return m_impl->update(buf, length);
}

auto md5::finish() -> string
{
  return m_impl->finish();
}

////////////////////////////////////////////////////////////////////////////////
//
// static one shot interface
//
////////////////////////////////////////////////////////////////////////////////
string md5::hash(string const& in)
{
  string result(0, 16);

  ::mbedtls_md5((uint8_t *)in.c_str(), in.size(), 
      (uint8_t*)result.data());

  return result;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
