#include <fstream>
#include <iostream>
#include <string>
#include <cstdlib>
#include <cassert>
#include <map>

#include "file_util.hpp"
#include "byte_buffer.hpp"

using namespace std;
using namespace sys::io;
      namespace file_io = util::file;

auto ivf_check_header(byte_buffer& bb) -> bool
{
  auto fourcc = bb.get_ascii(4);
  if (fourcc != "DKIF")
    return false;

  auto ver = bb.get_uint16_le();
  if (ver != 0)
    return false;

  auto header_len = bb.get_uint16_le();
  if (header_len != 0x20)
    return false;

  auto codec = bb.get_ascii(4);
  if (codec != "AV01")
    return false;

  bb.skip(0x14);
  return bb.offset() == 0x20;
}

auto get_frame_offsets(byte_buffer& bb) -> map<long, uint32_t>
{
  map<long, uint32_t> result;
  long offset = 0x20;
  for (int i=1; bb.has_remaining(); i++)
  {
    auto size = bb.get_uint32_le(); 
    bb.skip(8);

    offset += 12;

    result[offset] = size;

    bb.skip((int)size);
    offset += size;
  }

  return result;
}

int main(int argc, char *argv[])
{
  auto base = "/Users/yielding/work/z.RND/av1.carving/avi1.ivf"s;

  string fname = (argc == 2)
    ? string(argv[1]) 
    : base + "/img_Johnny_1280x720.ivf";

  ifstream file(fname);
  if (!file.is_open()) { cout << "err"; exit(EXIT_FAILURE); }

  ofstream ofs(base + "/codec_stream.bin");
  if (!ofs.is_open()) { cout << "err"; exit(EXIT_FAILURE); }

  int sz = file_io::size(file);
  auto content = file_io::read_sp(file, 0, sz);
  byte_buffer bb(content.get(), 0, sz);

  assert(ivf_check_header(bb));

  auto res  = get_frame_offsets(bb);
  auto here = (char*)bb.pointer();
  for (auto& e: res)
  {
    cout << e.first << " : " 
         << e.second << endl;

    ofs.write(here + e.first, e.second);
  }
  
  return 0;
}