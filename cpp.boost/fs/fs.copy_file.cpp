#include <iostream>
#include <fstream>
#include <stdexcept>
#include <boost/filesystem.hpp>

bool copy_file(std::string const& src, std::string const& dest)
{
  if (!fs::exists(src) || !fs::is_regular_file(src))
    return false;

  string const& dest_path = fs::path(dest).parent_path().c_str();

  if (!fs::exists(dest_path))
  {
    bool ok = fs::create_directories(dest_path);
    if (!ok)
      throw std::runtime_error("fail to create " + dest_path);
  }

  int64_t file_size = fs::file_size(src);
  int64_t written = 0;
  ifstream fi; fi.open(src.c_str(), ios_base::binary);
  ofstream fo; fo.open(dest.c_str(), ios_base::binary);
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
    throw std::runtime_error("size mismatches");

  return true;
}

int main(int argc, char const* argv[])
{
  std::string  src = "/Users/yielding/Desktop/scratch.rb";
  std::string dest = "/tmp/a/b/c/d/e/f/t.txt";

  std::cout << (copy_file(src, dest) ? "ok\n" : "fail\n");

  return 0;
}
