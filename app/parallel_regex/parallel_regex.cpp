#include "parallel_regex.h"
#include <boost/scoped_array.hpp>
#include <fstream>
#include <iostream>

using namespace std;

namespace {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template<class T>
T merge_copy(const T& a, const T& b)
{
  T ret(a);
  copy(b.begin(), b.end(), back_inserter(ret));

  return ret;
}

template<class T>
T nuke_dupes_no_sort_copy(const T& c)
{
  T ret;
  for (typename T::const_iterator i = c.begin(); i != c.end(); ++i)
    if (find(ret.begin(), ret.end(), *i) == ret.end())
      ret.push_back(*i);

  return ret;
}

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct RegexContinuation: public tbb::task
{
  RegexContinuation(matches& res) : result(res)
  {}

  task* execute()
  {
    result = merge_copy(left, right);
    return NULL;
  }

  matches& result;
  matches  right;
  matches  left;
};

RegexTask::RegexTask(string const& fn, int64_t beg, int64_t end, matches& res)
  : m_filename(fn)
  , m_beg(beg)
  , m_end(end)
  , m_result(res)
  , m_regex(beg, end - beg, BUFFER_SIZE)
{}

void RegexTask::serial_regex()
{
  std::ifstream in; in.open(m_filename.c_str(), ios_base::binary | ios_base::in);
  if (!in.is_open())
  {
    cout << "stream not opened\n";
    return;
  }
  else
  {
    cout << "open ok, offset: " << m_beg << " " << endl;
  }

  in.seekg(m_beg, ios_base::beg);
  if (m_regex.search(boost::regex("\xff\xff"), in, false))
  {
    m_result.swap(m_regex.m_results);
    cout << "swaped! : "  << m_result.size() << " offset: " << m_beg << endl;
  }
}

tbb::task* RegexTask::execute()
{
  if (m_end - m_beg < CUT_OFF)
  {
    serial_regex();
  }
  else
  {
    RegexContinuation& c = *new (allocate_continuation()) RegexContinuation(m_result);

    int64_t mid = m_beg + (m_end - m_beg) / 2;
    RegexTask& a = *new(c.allocate_child()) RegexTask(m_filename, m_beg, mid, c.left);
    RegexTask& b = *new(c.allocate_child()) RegexTask(m_filename, mid, m_end, c.right);
    c.set_ref_count(2);

    spawn(b);
    spawn(a);
  }

  return NULL;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool PartialRegex::search(boost::regex const& e, std::istream& is, bool active)
{
  using namespace boost; 
  scoped_array<char> buf(new char[m_buffer_size]);
  char* const     pbuf = buf.get();
  char const* next_pos = pbuf + m_buffer_size;

  matches results;
  int64_t match_count = 0;
  int64_t total_read  = 0;

  bool ok = true;
  while (ok)
  {
    // 0. determine whether or not we should go on
    if (should_stop())
      return false;

    // 1. compact partial matched string to buffer[0] if available
    //    invariant: left_over + next_pos == (buf + sizeof(buf))
    ptrdiff_t left_over = (pbuf + m_buffer_size) - next_pos;
    memmove(pbuf, next_pos, left_over);

    // 2. read text from the outer stream
    ptrdiff_t to_read = next_pos - pbuf;
    is.read(pbuf + left_over, to_read);

    // 3. update the execution condition
    std::streamsize read = is.gcount();
    total_read += read;
    ok = (read == to_read) && (total_read < m_stream_size);

    // 4. increment offsets we read so far
    m_offset += read;

    // 5. reset next position
    next_pos = pbuf + m_buffer_size;

    // 6. search
    cregex_iterator cur(pbuf, pbuf+read+left_over, e, match_default|match_partial);
    cregex_iterator last;

    // 6. no match.
    //    prepare next position if no match occurred
    if (cur == last)
      continue;

    // 6. match found!
    //    discriminate partial vs. complete
    while (cur != last)
    {
      // 6.1) partial match
      if ((*cur)[0].matched == false)  
      {
        next_pos = (*cur)[0].first;
        break;
      }

      // 6.2) full match
      int64_t match_length = (*cur)[0].second - (*cur)[0].first;
      int64_t       offset = (*cur)[0].first  - pbuf;
      int64_t  base_offset = m_offset - read - left_over;
      uint32_t group_index = 0;

      if (cur->size() > 1)
      {
        for (size_t i=0; i<cur->size(); i++)
        {
          string const& s = (*cur)[i].str();
          if (!s.empty())
          {
            group_index = i;
            break;
          }
        }
      }

      m_results.push_back(match_result(base_offset + offset, match_length, group_index));

      ++cur;
    }
  }

  return !m_results.empty();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
