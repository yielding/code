#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

int main(int argc, char *argv[])
{
  auto& cp = ::ranges::v3::copy;

  auto v1 = views::ints(0, 100) | to<vector>();
  auto v2 = v1 | cp | actions::stride(10);
  cout << views::all(v2) << endl; // {0, 10, 20, 30, ..., 90}

  v2 |= actions::stride(4);
  cout << views::all(v2) << endl;

  v2 |= actions::stride(2);
  cout << views::all(v2) << endl;

  v2 |= actions::stride(10);
  cout << views::all(v2) << endl;
  
  return 0;
}