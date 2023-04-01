#include <iostream>
#include <algorithm>
#include <string>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>
#include <range/v3/algorithm/copy.hpp>

#include <boost/format.hpp>

using namespace std;
using namespace boost;
      namespace view = ::ranges::views;

struct user
{
  string name;
  int age;

  auto to_s() const -> string 
  {
    return str(format("name: %s, age: %d") % name % age);
  }
};

int main(int argc, char *argv[])
{
  vector<user> users 
    = { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };

  auto result = users
    | view::filter   ([](auto& u) { return u.age > 18; })
    | view::transform([](auto& u) { return u.age; });

  cout << view::all(result);
  
  return 0;
}
