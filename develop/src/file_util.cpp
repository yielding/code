#include <boost/endian/buffers.hpp>
#include <fstream>

using namespace std;

namespace util::file {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool exists(string const& path)
{
  return ifstream{path}.good();
}

bool exists(char const* path)
{
  return ifstream{path}.good();
}

auto size(const char* filename) -> long
{
  ifstream ifs(filename, ios::ate | ios::binary);

  return long(ifs.tellg());
}

auto size(ifstream& ifs) -> long
{
  if (!ifs.good())
    return 0;

  auto backup_pos = ifs.tellg();
  ifs.seekg(0, ios::end);
  auto result = ifs.tellg();

  ifs.seekg(backup_pos, ios::beg);

  return long(result);
}

auto read(std::istream& in, long offset, int size) -> uint8_t*
{
  in.seekg(offset);
  auto buffer = new uint8_t[size];
  in.read((char*)buffer, size);

  return buffer;
}

auto uint4_be(std::istream& in, long offset) -> uint32_t
{
  in.seekg(offset);
  uint32_t result;
  in.read(reinterpret_cast<char*>(&result), sizeof(uint32_t));
  
  return boost::endian::endian_reverse(uint32_t(result));
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
