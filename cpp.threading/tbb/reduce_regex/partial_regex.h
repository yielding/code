#ifndef ACTIVE_REGEX_H_BC00NK15
#define ACTIVE_REGEX_H_BC00NK15

#include "tbb/tick_count.h"
#include "tbb/blocked_range.h"

#include <boost/regex.hpp>
#include <vector>
#include <string>
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

class PartialRegex
{
public:
  PartialRegex(int64_t offset, int64_t ss, int64_t bs = 64 * 1024) 
    : m_offset(offset)
    , m_stream_size(ss)
    , m_buffer_size(bs)
  {}

  bool     search(boost::regex const& e, std::istream& is);
  matches  result() { return m_results; }

  void     buffer_size(int64_t s) { if (s >= 10) m_buffer_size = s; }
  int64_t  buffer_size()          { return m_buffer_size;           }

private:
  int64_t  m_offset;
  int64_t  m_stream_size;
  int64_t m_buffer_size;

public:
  matches  m_results;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
