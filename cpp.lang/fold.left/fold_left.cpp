#include <iostream>

using namespace std;

template <typename ...Args>
int sum_all1(Args&&... args)
{
  return (args + ... + 0);
}

template <typename... Ints>
int sum_all2(Ints... nums)
{
  return (... + nums);
}

template <typename Int, typename... Ints>
Int diff_from(Int start, Ints... nums)
{
  return (start - ... - nums);
}

int main(int argc, char *argv[])
{
  cout << sum_all1(1, 2, 3, 4) << endl;
  cout << sum_all2(1, 2, 3, 4) << endl;
  cout << diff_from(100, 1, 2, 3) << endl;

  return 0;
}
