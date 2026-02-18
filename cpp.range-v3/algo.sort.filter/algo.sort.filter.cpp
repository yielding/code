#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>

#include <format>
#include <vector>
#include <iostream>

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

auto main(int argc, char* argv[]) -> int
{
  using v::filter, v::transform, v::all;

  auto users = vector<user>{{"leech", 49}, {"kamin", 47}, {"gunhee", 17}};
  auto result = users | filter([](auto& u) -> auto { return u.age > 18; }) 
                      | transform([](auto& u) -> auto { return u.age; });

  cout << all(result);

  return 0;
}
