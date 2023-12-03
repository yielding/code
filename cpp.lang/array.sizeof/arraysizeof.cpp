#include <iostream>

template <typename T, size_t N>
char (&Helper(T (&array)[N]))[N];

#define arraysizeof(array) (sizeof(Helper(array)))

using namespace std;

void f(int arr[])
{
  cout << sizeof(arr) /sizeof (arr[0]) << endl;
  cout << arraysizeof(arr) << endl;
}

int main(int argc, char const* argv[])
{
  int arr[10];

  std::cout << arraysizeof(arr) << endl;
  f(arr);

  return 0;
}
