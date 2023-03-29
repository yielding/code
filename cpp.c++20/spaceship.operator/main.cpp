#include <iostream>
#include <compare>
#include <string>
#include <tuple>
#include <set>

using namespace std;

struct Human {
  Human(int age, const char* name)
    : m_age(age), m_name(name)
  {}

  auto operator<=>(const Human& rhs) const { // auto == strong_ordering
    return m_age <=> rhs.m_age;
  }

  auto operator==(const Human& rhs) const {
    // 객체의 ordering 과 equality는 다르게 생각해야 한다.
    // 간단히는 아래와 같이도 생각할 수 있으나, 사용자 마음인 것
    // i.e, 두 string을 비교한다면 먼저 길이로 비교하는 것이 각 
    // 원소를 각각 비교하는 것보다 간명하다. 여기서는 
    return (*this <=> rhs) == 0;
  }

  string m_name;
  int m_age;
};

int main(int argc, char *argv[])
{
  Human a(10, "leech");
  Human b(10, "kamin");

  set<Human> humans;
  humans.insert(a);
  humans.insert(b);

  for (auto const& e: humans)
    cout << e.m_name << endl;


  /*
  auto res = a == b;

  cout << (a < b) << endl;
  cout << (a > b) << endl;
  cout << (a <=> b == 0) << endl;
  cout << (a == b) << endl;
  */

  return 0;
}