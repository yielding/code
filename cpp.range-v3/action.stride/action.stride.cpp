#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

int main(int argc, char *argv[])
{
  auto& cp = ::ranges::v3::copy;
  auto pr = [](auto && r) { cout << views::all(r) << endl; };

  auto v1 = views::ints(0, 100) | to<vector>();
  auto v2 = v1 | cp | actions::stride(10);
  pr(v2);

  v2 |= actions::stride( 4); pr(v2);
  v2 |= actions::stride( 2); pr(v2);
  v2 |= actions::stride(10); pr(v2);
  
  return 0;
}