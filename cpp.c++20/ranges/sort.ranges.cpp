#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <ranges>

#include <boost/format.hpp>

using namespace std;
using namespace boost;

struct user
{
  string name;
  int age;

  auto to_s() const -> string 
  {
    // return format("name: {}, age, {}", name, age);
    return str(format("name: %s, age: %d") % name % age);
  }
};

int main(int argc, char *argv[])
{
  vector<user> users = { {"leech", 49}, {"kamin", 47} };
  auto sort_by_name = [](auto& l, auto& r) {
    return l.age < r.age;
  };

  ranges::sort(users, sort_by_name);
  for (auto& u: users)
    cout << u.to_s() << endl;
  
  return 0;
}
