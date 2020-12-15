#include <iostream>
#include <type_traits>
#include <stack>

using namespace std;

template <typename T>
concept Equal = requires(T a, T b) {
    { a == b } -> __detail::__boolean_testable;
    { a != b } -> __detail::__boolean_testable;
};

bool areEqual(Equal auto a, Equal auto b)
{
  return a == b;
}

int main(int argc, char *argv[])
{
  stack<int> s;
  
  cout << boolalpha << endl;

  cout << "areEqual(1, 5): " << areEqual(1, 5) << endl;

  return 0;
}
