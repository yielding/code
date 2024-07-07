#include <iostream>
#include <format>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>

using namespace ranges::v3;
using namespace std;

struct User
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
  vector<User> users 
    = { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };

  auto result = users
    | view::filter   ([](auto& u) { return u.age > 18; })
    | view::transform([](auto& u) { return u.age; });

  cout << view::all(result);
  
  return 0;
}
