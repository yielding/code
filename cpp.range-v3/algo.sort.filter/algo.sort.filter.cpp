#include <iostream>
#include <format>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>

namespace v = ranges::views;
using namespace std;

struct user
{
  string name;
  int age;

  auto to_s() const 
  {
    return format("name: {}, age: {}", name, age);
  }
};

int main(int argc, char *argv[])
{
  using v::filter, v::transform, v::all;

  vector<user> users 
    = { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };

  auto result = users
    | filter   ([](auto& u) { return u.age > 18; })
    | transform([](auto& u) { return u.age; });

  cout << all(result);
  
  return 0;
}
