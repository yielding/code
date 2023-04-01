#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <ranges>
#include <compare>

#include <boost/format.hpp>

using namespace std;

struct user
{
  string name;
  int age;

  auto to_s() const -> string 
  {
    using namespace boost;
    return str(format("name: %s, age: %d") % name % age);
  }
};

int main(int argc, char *argv[])
{
  vector<user> users { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };

  auto by_name = [](auto& l, auto& r) { return l.name < r.name; };
  auto by_age  = [](auto& l, auto& r) { return l.age < r.age; };

  ranges::sort(users, by_name);
  for (auto& u: users) cout << u.to_s() << endl;

  cout << endl;
  
  ranges::sort(users, by_age);
  for (auto& u: users) cout << u.to_s() << endl;

  return 0;
}
