#ifndef FILEBASE_DEVICE_H_SCJ8SG1F
#define FILEBASE_DEVICE_H_SCJ8SG1F

#include <boost/iostreams/categories.hpp>   // seekable_device_tag
// #include <boost/iostreams/detail/ios.hpp>   // exceptions
#include <iosfwd>                           // streamsize, seekdir
#include <vector>
#include <stdint.h>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#if 1
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif

namespace io = boost::iostreams;

class FileBaseDevice
{
public:
  typedef char                    char_type;
  typedef io::seekable_device_tag category;

public:
  FileBaseDevice(FileBase* fb, int offset = 0)
    : m_fb(fb)
    , m_pos(offset)
  {}

  std::streamsize read(char_type* s, std::streamsize n)
  {
    using namespace std;
    streamsize    amt = static_cast<streamsize>(m_fb->GetSize() - m_pos);
    streamsize result = (min)(n, amt);

    if (result == 0 || !m_fb->GetData((unsigned char*)s, n, m_pos))
      return -1;

    m_pos += n;
    return result;
  }

  std::streamsize write(char_type const* s, std::streamsize n)
  {
    return -1;
  }

  std::streampos seek(std::streampos off, std::ios_base::seekdir way)
  {
    std::streampos next;

    if (way == std::ios_base::beg)
      next = off;
    else if (way == std::ios_base::cur)
      next = m_pos + off;
    else if (way == std::ios_base::end)
      next = m_fb->GetSize() + off;
    else
      throw std::ios_base::failure("bad seek direction");

    if (next < 0 || next > m_fb->GetSize())
      throw std::ios_base::failure("bad seek offset");

    m_pos = next;

    return m_pos;
  }

private:
  FileBase* m_fb;
  std::streamsize m_pos;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#if 0
#include <boost/iostreams/stream.hpp>  // io::stream

int main(int argc, char const* argv[])
{
  FileBase* fb = new FileBase;
  FileBaseDevice fbs(fb);
  io::stream<FileBaseDevice> in(fbs);

  m_regex.buffer_size(1024*16);
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  matches const& r = m_regex.result();
  assert(r.size() == 2);
  assert(r[0].offset == 0);
  assert(r[0].length == 2);
  assert(r[1].offset == 1024*1024-2);
  assert(r[1].length == 2);

  delete fb;

  return 0;
}
#endif
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
