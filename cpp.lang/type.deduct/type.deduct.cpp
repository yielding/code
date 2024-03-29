#include <iostream>

using namespace std;

template <typename T>
struct SelfType
{
  typedef T type;
};

void add(int& outNumber)
{
  ++outNumber;
}

template<typename R, typename A1>
R exec(R (*pfunc)(A1), typename SelfType<A1>::type arg1)
{
  return pfunc(arg1);
}

int main()
{
  int number = 10;
  exec(add, number);  // <-- compiler OK
  cout << number;
}
