#include <boost/endian/detail/disable_warnings.hpp>
#include <boost/endian/buffers.hpp>
#include <boost/static_assert.hpp>

#include <fstream>
#include <iostream>
#include <string>

using namespace std;
using namespace boost::endian;

namespace 
{
  struct header
  {
    big_int32_buf_at    file_code;
    big_int32_buf_at    file_length;
    little_int32_buf_at version;
    little_int32_buf_at shape_type;
  };

  char const* filename = "test.dat";

}

int main(int argc, char *argv[])
{
  header h;

  h.file_code   = 0x01020304;
  h.file_length = sizeof(header);
  h.version     = 1;
  h.shape_type  = 0x01020304;

  ofstream ofs;
  ofs.open(filename, ios_base::binary);

  ofs.write(reinterpret_cast<char*>(&h), sizeof h);
  ofs.close();

  cout << ::endian_reverse((int16_t)0x01);
  
  return 0;
}
