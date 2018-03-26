#include <iostream>
#include <string>
#include <set>
#include <tuple>

using namespace std;

struct S
{
  int n;
  string s;
  float d;

  bool operator<(const S& rhs) const
  {
    return tie(n, s, d) < tie(rhs.n, rhs.s, rhs.d);
  }
};

int main(int argc, const char *argv[])
{
  set<S> set_of_s;

  int* s = nullptr;

  S value{42, "Test", 3.14};
  set<S>::iterator iter;
  bool inserted;

  // unpacks the return value of insert into iter and inserted
  tie(iter, inserted) = set_of_s.insert(value);

  if (inserted)
    cout << "value was inserted successfully\n";
}
