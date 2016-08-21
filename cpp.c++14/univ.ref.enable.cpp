#include <iostream>
#include <string>
#include <type_traits>

using namespace std;

class Person
{
public:
  template <
    typename T,
    typename = enable_if_t<
      !is_base_of<Person, decay_t<T>>::value
      &&
      !is_integral<remove_reference_t<T>>::value
    >
  >
  explicit Person(T&& n): name(forward<T>(n))
  {
    cout << name;
  }

  explicit Person(int index): name(nameFromIdx(index))
  {
    cout << index;
  }

private:
  string nameFromIdx(int index)
  {
    return ""s;
  }

private:
  string name;
};

int main(int argc, char *argv[])
{
  Person s1("leech");
  
  return 0;
}
