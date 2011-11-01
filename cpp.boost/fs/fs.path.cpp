#include <boost/filesystem/operations.hpp>
#include <iostream>

using namespace std;
namespace fs = boost::filesystem;

int main(int argc, char const* argv[])
{
  fs::path p = fs::temp_directory_path();
  cout << p.string() + "x.dat";

  return 0;
}
