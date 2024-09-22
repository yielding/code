#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  using v::closed_iota, v::sliding, v::chunk, v::all;

  auto r1 = closed_iota(1, 6);
  auto r2 = r1 | sliding(2); // [[1,2],[2,3],[3,4],[4,5],[5,6]]
  auto c2 = r1 | chunk(2);   // [[1,2],[3,4],[5,6]]

  cout << all(r1) << endl;
  cout << all(r2) << endl;
  cout << all(c2) << endl;

  return 0;
}