#include <iostream>
#include <algorithm>
#include <functional> 
<<<<<<< HEAD
#include <ranges>
=======
#include <string>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>
#include <range/v3/algorithm/copy.hpp>

>>>>>>> c3fddd45e5e4d1aef6f96167950761bb542f17b8
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

<<<<<<< HEAD
  ranges::copy(result, ostream_iterator<int>(cout, "\n"));
  // cout << ranges::views::all(result);
=======
  copy(result, ostream_iterator<int>(cout, "\n"));
  cout << views::all(result);
>>>>>>> c3fddd45e5e4d1aef6f96167950761bb542f17b8
  
  return 0;
}
