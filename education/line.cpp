#include <fstream>
#include <iostream>
#include <string>

using namespace std;

int main(int argc, const char *argv[])
{
  if (argc != 2)
  {
    cout << "./line ./source.cpp\n";
    return EXIT_FAILURE;
  }

  auto in_file = std::string(argv[1]);

  ifstream ifs(in_file);
  ofstream ofs(in_file + ".line");

  string line;
  for (int no=0; getline(ifs, line); ++ no)
  {
    char buf[1024] = { 0 };
    sprintf(buf, "%5d: %s\n", no, line.c_str());
    cout << buf;
    ofs  << buf;
  }
  
  return 0;
}
