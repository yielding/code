#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/algorithm/copy.hpp>
#include <range/v3/all.hpp>

using namespace ranges;
using std::cout, 
      std::endl;

int main(int argc, char *argv[])
{
  auto v1 = views::ints(0, 100) | to<std::vector>();

  auto v2 = v1 | copy | actions::stride(10);
  cout << views::all(v2) << endl; // {0, 10, 20, 30, ..., 90}

  v2 |= actions::stride(4);
  cout << views::all(v2) << endl;

  v2 |= actions::stride(2);
  cout << views::all(v2) << endl;

  v2 |= actions::stride(10);
  cout << views::all(v2) << endl;
  
  return 0;
}