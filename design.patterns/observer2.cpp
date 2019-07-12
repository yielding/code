#include <iostream>
#include <string>
#include <boost/signals2.hpp>

using namespace std;
using namespace boost;
using namespace signals2;

template <typename T>
class Observerable
{
public:
  virtual ~Observerable() = default;
  signal<void(T&, const string&)> property_changed;
};

struct Person: Observerable<Person>
{
  explicit Person(int age) :age{age} {}

  int get_age() const { return age; }
  void set_age(int age)
  {
    if (this->age == age)
      return;

    this->age = age;
    property_changed(*this, "age");
  }

private:
  int age;
};

int main(int argc, char *argv[])
{
  Person p(123);
  p.property_changed.connect([](Person&, const string& pname) {
      cout << pname << " has been changed" << endl;
  });

  p.set_age(20);
  getchar();
  
  return 0;
}
