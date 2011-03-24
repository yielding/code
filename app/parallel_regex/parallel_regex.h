#ifndef ACTIVE_REGEX_H_BC00NK15
#define ACTIVE_REGEX_H_BC00NK15

#include "long_runnable.h"
#include "tbb/tick_count.h"
#include "tbb/task.h"
#include "tbb/task_scheduler_init.h"

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

class PartialRegex: public long_runnable<void, match_result>
{
public:
  PartialRegex()
  {}

  bool     search(boost::regex const& e, std::istream& is, bool active=false);
  matches  result() { return m_results; }

  void     buffer_size(uint32_t s) { if (s >= 10) m_buffer_size = s; }
  uint32_t buffer_size()           { return m_buffer_size;           }

private:
  int64_t  m_offset;
  uint32_t m_buffer_size;

  matches  m_results;
};

namespace tbb { class task; }

class RegexTask: public tbb::task
{
public:
  enum { CUT_OFF = 32 * 1024, BUFFER_SIZE = 64 * 1024 };

public:
  RegexTask(std::string const& fn, int64_t offset, int32_t ss, matches* res)
    : m_filename(fn)
    , m_offset(offset) 
    , m_stream_size(ss)
    , m_result(res)
  {}

  tbb::task* execute();

private:
  std::string m_filename;
  int32_t  m_stream_size;
  int64_t  m_offset;
  matches* m_result;
  PartialRegex m_regex;

};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
