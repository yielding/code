#include<boost/filesystem/operations.hpp>
#include<iostream>

namespace bf = boost::filesystem;         //create an alias

int main()
{
  bf::path p("fs.simple.ls.cpp");

  if (bf::exists(p))
    std::cout << p.leaf() << " exists.\n";
  else
    std::cout << p.leaf() << " does not exist.\n";
}
