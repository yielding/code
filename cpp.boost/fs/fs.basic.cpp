#include <boost/filesystem/operations.hpp>
#include <iostream>

namespace fs = boost::filesystem;         //create an alias

int main()
{
  fs::path p("fs.simple.ls.cpp");

  if (fs::exists(p))
    std::cout << p.leaf() << " exists.\n";
  else
    std::cout << p.leaf() << " does not exist.\n";
}
