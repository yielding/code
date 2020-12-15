#include <iostream>
#include <stdexcept>
#include <filesystem>
#include <fstream>
//#include <boost/filesystem.hpp>

using namespace std;
namespace fs = std::filesystem;

bool copy_file(string const& src, string const& dest)
{
  if (!fs::exists(src) || !fs::is_regular_file(src))
  {
    cout << "wrong input" << endl;
    return false; 
  }

  auto const& dest_path = fs::path(dest).parent_path().string();
  cout << dest_path << endl;

  if (!fs::exists(dest_path))
  {
    bool ok = fs::create_directories(dest_path);
    if (!ok)
      throw runtime_error("fail to create " + dest_path);
  }

  int64_t file_size = fs::file_size(src);
  int64_t written = 0;
  ifstream fi; fi.open(string(src) , ios_base::binary);
  ofstream fo; fo.open(string(dest), ios_base::binary);
  while (written < file_size)
  {
    int const BUF_SIZE = 64 * 1024;
    char buffer[BUF_SIZE] = { 0 };
    fi.read(buffer, BUF_SIZE);
    auto read = fi.gcount();
    if (read <= 0)
      break;

    fo.write(buffer, read);
    written += read;
  }

  if (written != file_size)
    throw runtime_error("size mismatches");

  return true;
}

int main(int argc, char const* argv[])
{
  string  src = "/Users/yielding/Desktop/deep.pdf";
  string dest = "/tmp/a/b/c/d/e/f/deep.pdf";

  cout << (copy_file(src, dest) ? "ok\n" : "fail\n");

  return 0;
}
