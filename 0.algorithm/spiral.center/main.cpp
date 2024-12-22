#include <vector>
#include <print>

using namespace std;

template<typename T> using dir = pair<T, T>;
template<typename T> using dirs = vector<dir<T>>;
template<typename T> using matrix = vector<vector<T>>;

struct RotateR 
{
  static auto next_dir() -> dir<int>
  {
    static dirs<int> _dirs { {1, 0}, {0, 1}, {-1, 0}, {0, -1} };
    auto res = _dirs[0]; 
    rotate(_dirs.begin(), _dirs.begin() + 1, _dirs.end());

    return res;
  }
};

struct RotateL 
{
  static auto next_dir() -> dir<int>
  {
    static dirs<int> _dirs { {-1, 0}, {0, 1}, {1, 0}, {0, -1} };
    auto res = _dirs[0]; 
    rotate(_dirs.begin(), _dirs.begin() + 1, _dirs.end());

    return res;
  }
};

template <typename RotatePolicy>
class SpiralArray
{
public:
  explicit SpiralArray(int dim=5, const int val=1)
    : _board{0}, _value {val}
  {
    for (int i=0; i<dim; i++) _board.emplace_back(dim);

    _board[dim/2][dim/2] = val;
  }

public:
  void go()
  {
    const auto dim = _board.size();
    auto cur = make_pair(dim / 2, static_cast<int>(dim / 2));
    auto val = _value;
    for (int step = 1; ; ++step)
    {
      cur = update_board(cur, RotatePolicy::next_dir(), step, val);
      if (val >= dim * dim) return;

      cur = update_board(cur, RotatePolicy::next_dir(), step, val);
      if (val >= dim * dim) return;
    }
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
  auto update_board(auto cur, auto dir, int step, int& v) -> pair<int, int>
  {
    auto [x0, y0] = cur;
    auto [dx, dy] = dir;

    for (auto i = 0; i < step; i++)
      _board[y0 += dy][x0 += dx] = ++v;

    return {x0, y0};
  }

private:
  matrix<int> _board;
  int _value;
};

int main(int argc, char* argv[])
{
  constexpr auto dim = 7;

  SpiralArray<RotateL> m0{dim};
  m0.go();
  m0.print_to_console();

  SpiralArray<RotateR> m1{dim};
  m1.go();
  m1.print_to_console();

  return 0;
}