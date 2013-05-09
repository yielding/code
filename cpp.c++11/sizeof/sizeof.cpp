#include <iostream>

using namespace std;

class X
{
public:
  int w;
};

int main(int argc, const char *argv[])
{
  cout << sizeof(X::w);
  
  return 0;
}
