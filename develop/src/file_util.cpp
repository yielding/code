#include <fstream>

using namespace std;

namespace util {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool file_exists(string const& path)
{
  return ifstream{path}.good();
}

bool file_exists(char const* path)
{
  return ifstream{path}.good();
}

auto file_size(const char* filename) -> long
{
  ifstream ifs(filename, ios::ate | ios::binary);

  return long(ifs.tellg());
}

auto file_size(ifstream& ifs) -> long
{
  if (!ifs.good())
    return 0;

  auto backup_pos = ifs.tellg();
  ifs.seekg(0, ios::end);
  auto result = ifs.tellg();

  ifs.seekg(backup_pos, ios::beg);

  return long(result);
}

auto file_read(std::istream& in, long offset, int size) -> uint8_t*
{
  in.seekg(offset);
  auto buffer = new uint8_t[size];
  in.read((char*)buffer, size);

  return buffer;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
