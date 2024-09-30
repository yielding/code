#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

void transpose()
{
  int const m[3][2] = {{1,2}, {3,4}, {5,6}};
  auto nc = g::distance(g::front(m));
  auto r = m | v::join;

  auto r_col = [r, nc](int col) {
    return r | v::drop(col) | v::stride(nc);
  };

  auto transp = v::iota(0, nc) | v::for_each(
      [r_col](int nc) { return g::yield(r_col(nc)); }
  );

  cout << v::all(r) << endl;
  cout << v::all(r_col(0)) << endl;
  cout << v::all(r_col(1)) << endl;
  cout << v::all(transp) << endl;
}

int main(int agc, char* agv[])
{
  int const m[2][3] = {{1, 2, 3}, {4, 5, 6}};
  auto nr = g::distance(m); // 2
  auto nc = g::distance(g::front(m)); // 3

  auto r  = m | v::join;
  auto c0 = r | v::drop(0) | v::stride(nc);
  auto c2 = r | v::drop(2) | v::stride(nc);
  auto dg = r | v::stride(nc + 1);

  cout << v::all(r) << endl;
  cout << v::all(c0) << endl;
  cout << v::all(c2) << endl;
  cout << v::all(dg) << endl;

  transpose();

  return 0;
}