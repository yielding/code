#include <iostream>
#include <compare>
#include <string>

using namespace std;

struct Human {
  Human(int age, const char* name)
    : _age(age), _name(name)
  {}

  auto operator<=>(const Human& rhs) const {
    return _age <=> rhs._age;
  }

  auto operator==(const Human& rhs) const {
    // 객체의 ordering 과 equality는 다르게 생각해야 한다.
    // 간단히는 아래와 같이도 생각할 수 있으나, 사용자 마음인 것
    return (*this <=> rhs) == 0;
  }

  string _name;
  int _age;
};

int main(int argc, char *argv[])
{
  Human a(10, "leech");
  Human b(10, "kamin");

  auto res = a == b;

  cout << (a < b) << endl;
  cout << (a > b) << endl;
  cout << (a <=> b == 0) << endl;
  cout << (a == b) << endl;

  return 0;
}

