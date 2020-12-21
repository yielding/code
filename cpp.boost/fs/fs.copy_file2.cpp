#include <iostream>
#include <stdexcept>
#include <filesystem>
#include <fstream>
#include <string>

using namespace std;
      namespace fs = std::filesystem;

int main(int argc, char const* argv[])
{
  string  src = "/Users/yielding/Desktop/deep.pdf";
  string dest = "/tmp/a/b/c/d/e/f";

  if (!fs::exists(dest))
  {
    auto ok = fs::create_directories(dest);
    if (!ok)
    {
      cout << "failed to create " << dest << endl;
      exit(EXIT_FAILURE);
    }
  }

  try
  {
    fs::copy_file(src, dest + "/deep.pdf");
  }
  catch(fs::filesystem_error& e)
  {
    cout << "Could not copy: " << e.what() << endl;
  }

  return 0;
}
