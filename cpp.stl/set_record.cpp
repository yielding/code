#include <set>
#include <iostream>
#include <string>

using namespace std;

struct data
{
  string name;
  int age;

  bool operator<(data const& rhs) const
  {
    return name < rhs.name;
  }
};

int main(int argc, char const* argv[])
{
  set<data> s;

  data a0, a1, a2;
  a0.name = "leech";
  a0.age  = 39;

  a1.name = "kamin";
  a1.age  = 37;

  a2.name = "kamin";
  a2.age  = 39;

  s.insert(a0);
  s.insert(a1);
  s.insert(a2);

  for (auto it=s.begin(); it != s.end(); ++it)
    cout << it->name << endl;

  return 0;
}
