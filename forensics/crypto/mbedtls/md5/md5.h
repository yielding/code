#ifndef MD5_H_WZHA1R4D
#define MD5_H_WZHA1R4D

#include <string>
#include <memory>

namespace mbedtls {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class md5
{
public:
  static auto hash(std::string const&) -> std::string;

public:
   md5();
  ~md5();

  md5(md5&& rhs) = default;
  auto operator=(md5&& rhs) -> md5& = default;

public:
  auto reset() -> void;
  auto update(std::string const&) -> void;
  auto update(uint8_t*, size_t len) -> void;

  auto finish() -> std::string;

private:
  struct impl;
  std::unique_ptr<impl> m_impl;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}

#endif
