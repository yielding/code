#include <string>
#include <vector>
#include <iostream>
#include <format>
#include <range/v3/all.hpp>

using namespace std;

struct user
{
  string name;
  int age;

  auto to_s() const -> string 
  {
    return format("name: {}, age: {}", name, age);
  }
};

auto main(int argc, char *argv[]) -> int
{
  using ::ranges::sort;

  auto users = vector<user> { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };

  auto by_name = [](auto& l, auto& r) -> auto { return l.name < r.name; };
  auto by_age  = [](auto& l, auto& r) -> auto { return l.age < r.age; };

  sort(users, by_name);
  for (auto& u: users) cout << u.to_s() << endl;

  cout << endl;
  
  sort(users, by_age);
  for (auto& u: users) cout << u.to_s() << endl;

  return 0;
}
