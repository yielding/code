#include <string>
#include <vector>
#include <algorithm>
#include <format>

using namespace std;

struct user
{
  string name;
  int age;

  auto to_s() const -> string 
  {
    return format("name: {}, age, {}", name, age);
  }
};

int main(int argc, char *argv[])
{
  vector<user> users = { {"leech", 49}, {"kamin", 47}};
  auto sort_by_name = [](auto& l, auto& r) {
    return l.age < r.age;
  };

  ranges::sort(users, sort_by_name);
  for (auto& u: user)
    cout << u.to_s() << endl;
  
  return 0;
}
