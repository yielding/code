#pragma once

#include <cstring>

using namespace std;

template <typename OnType>
class streamer
{
public:
  auto byte_count() -> int
  {
    auto const &s = self();

    return strlen(s.m_name) + strlen(s.m_value) + 2;
  }

private:
  auto self() -> OnType const&
  {
    auto const& self = static_cast<OnType const&>(*this);

    return self;
  }
};
