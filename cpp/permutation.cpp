#include <iostream>
#include <vector>
#include <iterator>
#include <numeric>
#include <functional>
#include <string>

using namespace std;

namespace
{
  template <typename T>
  void print(T const& col, char const* optstr="")
  {
    typename T::const_iterator pos;

    std::cout << optstr;
    for (pos=col.begin(); pos != col.end(); ++pos)
      std::cout << *pos << ' ';

    std::cout << std::endl;
  }

  template <typename T>
  void insert(T& col, int first, int last)
  {
    for (int i=first; i<=last; ++i)
      col.insert(col.end(), i);
  }
}

int main()
{
  std::vector<int> col;

  insert(col, 1, 5);
  print (col, "entry");

  while (next_permutation(col.begin(), col.end()))
    print(col, " ");

  print(col, "afterward: ");
}
