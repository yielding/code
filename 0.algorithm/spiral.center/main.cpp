#include <vector>
#include <print>

using namespace std;

template<typename T, typename U>
using pairs = vector<pair<T, U>>;

template<typename T>
using matrix = vector<vector<T>>;

class Matrix
{
public:
  explicit Matrix(int dim=5, int val=1) : _value {val}
  {
    for (int i=0; i<dim; i++) _board.emplace_back(dim);

    _board[dim/2][dim/2] = val;
  }

  void go()
  {
    auto dim = _board.size();
    auto cur = make_pair((dim / 2), int(dim / 2));
    auto val = _value;
    for (int step = 1; ; ++step)
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
    rotate(_dirs.begin(), _dirs.begin() + 1, _dirs.end());

    return res;
  }

  auto update_board(auto cur, auto dir, int step, int& v) -> pair<int, int>
  {
    auto [x, y] = cur;
    auto [dx, dy] = dir;

    for (auto i = 0; i < step; i++)
    {
      x += dx;
      y += dy;

      _board[y][x] = ++v;
    }

    return {x, y};
  }

  void print_to_console()
  {
    for (auto& row: _board)
    {
      for (auto cell: row) print("{:5}", cell);
      println("");
    }

    println("");
  }

private:
  matrix<int> _board;
  pairs<int, int> _dirs { {1, 0}, {0, 1}, {-1, 0}, {0, -1} };
  int _value;
};

int main(int argc, char* argv[])
{
  Matrix m{9};

  m.go();
  m.print_to_console();

  return 0;
}
