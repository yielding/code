#include <vector>
#include <algorithm>
#include <cstdio>
#include <iostream>

using namespace std;

class Matrix
{
public:
  explicit Matrix(int dim, int val) : _value {val}
  {
    for (int i=0; i<dim; i++) _board.emplace_back(dim);

    _board[dim/2][dim/2] = val;
  }

  void go()
  {
    auto dim = _board.size();
    auto cur = make_pair((dim / 2), int(dim / 2));
    auto val = _value;
    for (auto step = 1; ; ++step)
    {
      cur = update_board(cur, next_dir(), step, val);
      if (val >= dim * dim) return;

      cur = update_board(cur, next_dir(), step, val);
      if (val >= dim * dim) return;
    }
  }

  auto next_dir() -> pair<int, int>
  {
    auto res = _dirs[0];
    rotate(_dirs.begin(), _dirs.begin()+1, _dirs.end());

    return res;
  }

  auto update_board(auto cur, auto dir, int step, int& v) -> pair<int, int>
  {
    auto [x0, y0] = cur;
    auto [dx, dy] = dir;

    for (int i = 0; i < step; i++)
    {
      x0 += dx;
      y0 += dy;
      _board[y0][x0] = ++v;
    }

    return make_pair(x0, y0);
  }

  void print()
  {
    for (auto& row: _board)
    {
      for (auto cell: row) printf("%3d", cell);
      cout << endl;
    }

    cout << endl;
  }

private:
  vector<vector<int>> _board;
  vector<pair<int,int>> _dirs { {1, 0}, {0, 1}, {-1, 0}, {0, -1} };
  int _value;
};

int main(int argc, char* argv[])
{
  Matrix m(7, 1);

  m.print();
  m.go();
  m.print();

  return 0;
}