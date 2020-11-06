#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <functional> // #include <format>
#include <boost/format.hpp>

using namespace std;

struct user
{
  string name;
  int age;

  auto to_s() const -> string 
  {
    // return format("name: {}, age, {}", name, age);
    using namespace boost;
    return str(format("name: %s, age: %d") % name % age);
  }
};

bool underage(const user& u) { return u.age < 18; }

int main(int argc, char *argv[])
{
  vector<user> users = { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };
  auto result = users
    | views::filter(not_fn(underage))
    | views::transform([] (const auto& u) { return u.age; });

  
  ranges::copy(result, ostream_iterator<int>(cout, "\n"));
  
  return 0;
}
