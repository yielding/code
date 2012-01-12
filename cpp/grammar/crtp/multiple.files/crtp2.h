#ifndef CRTP2_H_NSZ7808K
#define CRTP2_H_NSZ7808K

#include <cstring>

using namespace std;

template <typename OnType>
class streamer
{
public:
  int byte_count();

private:
  OnType const& self();
};

template <typename OnType>
int streamer<OnType>::byte_count()
{
  OnType const &s = self();
  return strlen(s.m_name) + strlen(s.m_value) + 2;
}

template <typename OnType>
OnType const& streamer<OnType>::self()
{
  OnType const& self = static_cast<OnType const&>(*this);

  return self;
} 

#endif /* end of include guard: CRTP2_H_NSZ7808K */
