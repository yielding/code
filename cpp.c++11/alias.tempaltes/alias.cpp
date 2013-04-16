#include <iostream>

using namespace std;

template <typename A, typename B>
class AliasTemplate
{
public:
  A a;
  B b;
};

template <typename B> 
using AliasFixedA = AliasTemplate<int, B>;

int main(int argc, const char *argv[])
{
  AliasFixedA<char> b;

  cout << sizeof(b.a);
  cout << sizeof(b.b);

  return 0;
}
