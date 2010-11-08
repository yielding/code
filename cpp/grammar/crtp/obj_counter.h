#pragma once

////////////////////////////////////////////////////////////////////////////////
//
// CRTP의 전형
// Ruby는 이런 형식으로 Mixin을 사용한다. 
//    ex) Comparable
//
////////////////////////////////////////////////////////////////////////////////
namespace utility { namespace debug {

template <typename CountedType>
class object_counter
{
public:
  typedef object_counter<CountedType> self_t;

public:
  static size_t live()          { return self_t::m_count; }

protected:
  object_counter()              { ++self_t::m_count;      }
  object_counter(self_t const&) { ++self_t::m_count;      }
  ~object_counter()             { --self_t::m_count;      }

private:
   static size_t m_count;
};

template <typename CountedType>
size_t object_counter<CountedType>::m_count = 0;

} // end of debug
} // end of utility
