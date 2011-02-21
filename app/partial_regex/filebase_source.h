#ifndef FILEBASE_SOURCE_H_TJ7C7O0U
#define FILEBASE_SOURCE_H_TJ7C7O0U

#include <stdint.h>
#include <vector>
#include <boost/iostreams/concepts.hpp>  // source
#include <boost/iostreams/stream.hpp>

#include <algorithm>                       // copy, min
//#include <iosfwd>                          // streamsize
//#include <boost/iostreams/categories.hpp>

namespace io = boost::iostreams;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class FileBase
{
public:
  static int const SIZE = 1024 * 1024;

public:
  FileBase()
  {
    m_data.reserve(SIZE);
    for (int i=0; i<SIZE; ++i) 
      m_data[i] = i % 15 + 0xf0;

    m_data[0] = 0xff;
    m_data[1] = 0xff;
    m_data[SIZE-2] = 0xff;
    m_data[SIZE-1] = 0xff;
  }

  bool GetData(unsigned char* buf, int64_t size, int64_t offset)
  {
    if (size + offset > SIZE)
      return false;

    std::copy(m_data.begin()+offset, m_data.begin()+offset+size, buf);

    return true;
  }

  int64_t GetSize()
  {
    return SIZE;
  }

private:
  std::vector<uint8_t> m_data;
};

class FileBaseSource : public io::source
{
public:
  FileBaseSource(FileBase* fb)
    : m_fb(fb)
    , m_pos(0)
  {}

  std::streamsize read(char_type* s, std::streamsize n)
  {
    using namespace std;
    streamsize    amt = static_cast<streamsize>(m_fb->GetSize() - m_pos);
    streamsize result = (min)(n, amt);

    if (result == 0) 
      return -1;

    if (!m_fb->GetData((unsigned char*)s, n, m_pos))
      return -1;

    m_pos += n;
    return result;
  }

private:
  typedef size_t  size_type;
  FileBase* m_fb;
  std::streamsize m_pos;
};

//template <typename FileBase> 
//class FileBaseSource {
//public:
//  typedef char           char_type;
//  typedef io::source_tag category;
//
//  FileBaseSource(FileBase* fb)
//    : m_fb(fb)
//    , m_pos(0)
//  {}
//
//  std::streamsize read(char_type* s, std::streamsize n)
//  {
//    using namespace std;
//    streamsize    amt = static_cast<streamsize>(m_fb->GetSize() - m_pos);
//    streamsize result = (min)(n, amt);
//
//    if (result == 0) 
//      return -1;
//
//    if (!m_fb->GetData((unsigned char*)s, n, m_pos))
//      return -1;
//
//    m_pos += n;
//    return result;
//  }
//
//  FileBase& container() { return m_fb; }
//
//private:
//  typedef size_t  size_type;
//  FileBase* m_fb;
//  std::streamsize m_pos;
//};
//
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
