#include <iostream>
#include <fstream>
#include <iterator>

int main(int argc, char const* argv[])
{
  using namespace std;

  ifstream in("line_count.cpp");

  cout << count(istreambuf_iterator<char>(in), 
                istreambuf_iterator<char>(), '\n');

  return 0;
}
