#include <iostream>
#include <functional>
#include <string>

using namespace std;

struct S
{
  string first_name;
  string last_name;
};

template <typename T> class MyHash;

template <> 
class MyHash<S>
{
public:
  size_t operator()(S const& s) const
  {
    auto h1 = hash<string>()(s.first_name);
    auto h2 = hash<string>()(s.last_name);

    return h1 ^ (h2 << 1);
  }
};

int main(int argc, const char *argv[])
{
  string s1 = "Hubert";
  string s2 = "Farnsworth";
  hash<string> h1;

  S n1;
  n1.first_name = s1;
  n1.last_name  = s2;

  cout << "hash(s1) = " << h1(s1) << endl
       << "hash(s2) = " << hash<string>()(s2) << endl
       << "hash(n1) = " << MyHash<S>()(n1) << endl;
       
  
  return 0;
}
