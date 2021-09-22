#include <iostream>
#include <algorithm>
#include <functional> 
#include <string>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>
#include <range/v3/algorithm/copy.hpp>

#include <boost/format.hpp>

using namespace ranges;

using std::cout;
using std::vector;
using std::ostream_iterator;
using std::not_fn;

struct user
{
  std::string name;
  int age;

  auto to_s() const -> std::string 
  {
    using namespace boost;
    return str(format("name: %s, age: %d") % name % age);
  }
};

bool underage(const user& u) { return u.age < 18; }

int main(int argc, char *argv[])
{
  vector<user> users = { {"leech", 49}, {"kamin", 47}, {"gunhee", 17} };
  auto result = users
    | views::filter(std::not_fn(underage))
    | views::transform([] (const auto& u) { return u.age; });

  copy(result, ostream_iterator<int>(cout, "\n"));
  cout << views::all(result);
  
  return 0;
}
