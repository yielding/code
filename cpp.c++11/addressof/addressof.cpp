#include <cstddef>
#include <iostream>

using namespace std;

struct S {
  char c;
  double d;
};

int main(int argc, char const* argv[])
{
  cout << "the first element is at offset " << offsetof(S, c) << endl
       << "the double is at offset " << offsetof(S, d) << endl;
  
  return 0;
}
