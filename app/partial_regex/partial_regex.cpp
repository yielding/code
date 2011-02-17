#include "partial_regex.h"
#include <boost/scoped_array.hpp>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
PartialRegex::PartialRegex()
  : m_offset(0)
  , m_buffer_size(4096)
{}

bool PartialRegex::search(boost::regex const& e, std::istream& is)
{
  boost::scoped_array<char> buf(new char[m_buffer_size]);
  char* const pbuf = buf.get();

  char const* next_pos = pbuf + m_buffer_size;

  bool ok = true;

  bool partial = false;
  uint32_t partial_offset = 0;

  uint32_t buffer_count = 0;
  match_results results;

  while (ok)
  {
    // 1. compact partial matched string to buffer[0] if available
    //    invariant: left_over + next_pos == (buf + sizeof(buf))
    ptrdiff_t left_over = (pbuf + m_buffer_size) - next_pos;
    memmove(pbuf, next_pos, left_over);

    // 2. read text from the outer stream
    ptrdiff_t to_read = next_pos - pbuf;
    is.read(pbuf + left_over, to_read);

    // 3. update the execution condition
    std::streamsize read = is.gcount();
    ok = (read == to_read);
    ++buffer_count;

    // 4. search
    boost::cregex_iterator last;
    boost::cregex_iterator cur(pbuf, pbuf+read+left_over, e, boost::match_default|boost::match_partial);

    // 5. no match.
    //    prepare next position if no match occurred
    if (cur == last)
    {
      partial = false;
      partial_offset = 0;
      next_pos = pbuf + m_buffer_size;
      continue;
    }

    // 6. match
    //    discriminate partial vs. complete
    while (cur != last)
    {
      if ((*cur)[0].matched == false)
      {
        next_pos = (*cur)[0].first;
        partial  = true;
        partial_offset = ((buffer_count - 1) * m_buffer_size) + next_pos - buf.get();
        break;
      }
      else
      {
        m_offset = partial ? partial_offset
                           : (buffer_count - 1) * m_buffer_size + (*cur)[0].first  - buf.get();

        uint32_t match_length = (*cur)[0].second - (*cur)[0].first;
        m_results.push_back(match_result(m_offset, match_length));
        partial = false;
      }
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
