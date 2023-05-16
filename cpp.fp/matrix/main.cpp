#include <iostream>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;

using namespace std;

void transpose()
{
  int const m[3][2] = {{1,2}, {3,4}, {5,6}};
  auto nc = rg::distance(rg::front(m));
  auto r = m | rv::join;

  auto r_col = [r, nc](int col) {
    return r | rv::drop(col) | rv::stride(nc);
  };

  auto transp = rv::iota(0, nc) | rv::for_each(
      [r_col](int nc) { return rg::yield(r_col(nc)); }
  );

  cout << rv::all(r) << endl;
  cout << rv::all(r_col(0)) << endl;
  cout << rv::all(r_col(1)) << endl;
  cout << rv::all(transp) << endl;
}

int main(int argc, char* argv[])
{
  int const m[2][3] = {{1, 2, 3}, {4, 5, 6}};
  auto nr = rg::distance(m); // 2
  auto nc = rg::distance(rg::front(m));

  auto r  = m | rv::join;
  auto c0 = r | rv::drop(0) | rv::stride(nc);
  auto c2 = r | rv::drop(2) | rv::stride(nc);
  auto dg = r | rv::stride(nc + 1);

  cout << rv::all(r) << endl;
  cout << rv::all(c0) << endl;
  cout << rv::all(c2) << endl;
  cout << rv::all(dg) << endl;

  transpose();

  return 0;
}