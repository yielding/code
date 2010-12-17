#include <iostream>
#include <vector>

using namespace std;

int main(int argc, char const* argv[])
{
  int a[] = { 1, 2, 3, 4, 5, 6, 7,8, 9, 10};

  for (auto i : a)
    cout << i;

  return 0;
}
