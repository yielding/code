#ifndef ACTIVE_REGEX_H_BC00NK15
#define ACTIVE_REGEX_H_BC00NK15

#include "long_runnable.h"
#include <boost/regex.hpp>
#include <vector>
#include <stdint.h>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct match_result
{  
  match_result() {}

  match_result(int64_t o, uint32_t l, uint32_t g)
    : offset(o), length(l), group_index(g) 
  {}

  int64_t  offset;
  uint32_t length;
  uint32_t group_index;
};

typedef std::vector<match_result> matches;

class ActiveRegex: public long_runnable<void, match_result>
{
public:
  ActiveRegex();

  bool     search(boost::regex const& e, std::istream& is, bool active=false);
  matches  result() { return m_results; }

  void     buffer_size(uint32_t s) { if (s >= 10) m_buffer_size = s; }
  uint32_t buffer_size()           { return m_buffer_size;           }

private:
  int64_t  m_offset;
  uint32_t m_buffer_size;

  matches  m_results;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
