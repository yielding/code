#include <boost/filesystem/operations.hpp>


namespace fs = boost::filesystem;

#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  fs::path p = fs::temp_directory_path();
  cout << p.string() + "x.dat";

  return 0;
}
