#include <iostream>
#include <iomanip>
#include <set>
#include <string>
#include <compare>

using namespace std;

struct Human 
{
  Human(int age, string name)
    : m_age(age), m_name(move(name)) {}

  constexpr auto operator<=>(const Human& rhs) const 
  {
    return m_age <=> rhs.m_age;
  }

  string m_name;
  int m_age;
};

int main()
{
  Human a(20, "leech");
  Human b(10, "kamin");
  Human c(1, "gunhee");
  Human d(1, "gunhee2");

  set <Human> people{a, b, c, d};

  auto [_, res] = people.insert(d);

  cout << boolalpha << res << endl;

  for (auto v: people) cout << v.m_name << endl;

  return 0;
}