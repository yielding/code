#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include <cstdlib>
#include <algorithm>
#include <stdint.h>

#include <boost/filesystem.hpp>

using namespace std;
      namespace fs = boost::filesystem;
      
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace  
{
  /*
  streampos file_size(char const* file_path)
  {
    ifstream ifs(file_path, ios_base::binary);
    ifs.seekg(0, ios_base::end);

    return ifs.tellg();
  }
  */
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class JPEGImage 
{
public:
  JPEGImage(char const* path)
    : m_path(path)
    , m_header("\xFF\xD8\xFF")
    , m_tail("\xFF\xD9")
  {
    m_size = fs::file_size(m_path);

    m_buffer = new char[m_size];
    ifstream ifs(m_path, ios_base::binary);
    ifs.read(m_buffer, m_size);
  }

  ~JPEGImage()
  {
    delete [] m_buffer;
  }

  bool has_thumbnail()
  {
    auto pos = search(m_buffer+1, m_buffer+m_size, m_header, m_header+3);
    return pos != (m_buffer + m_size)
  }

  void save_tbnail_to(char const* path)
  {
    auto beg = search(m_buffer+1, m_buffer+m_size, m_header, m_header+3);
    auto end = search(m_buffer+1, m_buffer+m_size, m_tail,   m_tail+2);

    ofstream ofs(path, ios_base::binary);
    ofs.write(&*beg, distance(beg, end));
  }

private:
  char const* m_path;
  char* m_buffer;
  int64_t m_size;
  char const* m_header, m_tail;
};
  
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  assert(argc == 2);

  auto file_name = string(argv[1]);
  auto size      = fs::file_size(file_name);
  if (size < 0)
    exit(EXIT_FAILURE);

  JPEGImage j(file_name.c_str());

  if (j.has_thumbnail())
    j.save_tbnail_to("/Users/yielding/code/education/1.thm.jpg");

  return 0;
}
