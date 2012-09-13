#include <iostream>
#include <vector>
#include <string>
#include <map>

using namespace std;

int64_t cache[1000][1000];

class Pascal
{
public:
  auto value_of(int x, int y) -> int64_t;
  auto values_of(int row)     -> vector<int64_t>;
  auto line_of(int row)       -> string;
  auto lines_upto(int row)    -> string;
};

auto Pascal::value_of(int x, int y) -> int64_t
{
  if (x < 1 || y < 1 || y > x)
  {
    cache[x][y] = 0;
    return 0;
  }

  if (x == 1)
  {
    cache[x][y] = 1;
    return 1;
  }

  auto v1 = cache[x-1][y-1] ? cache[x-1][y-1] : value_of(x-1, y-1);
  auto v2 = cache[x-1][y  ] ? cache[x-1][y  ] : value_of(x-1, y  );

  if (cache[x][y] == 0)
    cache[x][y] = v1 + v2;

  return v1 + v2;
}

auto Pascal::values_of(int row) -> vector<int64_t>
{
  vector<int64_t> v;

  if (row > 0)
    for (int col=1; col<=row; col++)
      v.push_back(value_of(row, col));

  return v;
}

auto Pascal::line_of(int row) -> string
{
  string res;
  for (int i=1; i<=row; ++i)
  {
    char buf[1024] = { 0 };
    sprintf (buf, "%6lld", value_of(row, i));
    res += string(buf);
  }

  return res;
}

auto Pascal::lines_upto(int row) -> string
{
  string res;
  for (int i=1; i<=row; i++)
    res += string(3*(row-i), ' ') + line_of(i) + "\n";

  return res;
}

int main(int argc, const char *argv[])
{
  Pascal p;
                                            //   d   s   d
  cout << p.value_of(1, 1) << endl;         //   1 : 1 :
  cout << p.value_of(3, 2) << endl;         //   2 : 3 : 1   1x1
  cout << p.value_of(5, 3) << endl;         //   3 : 5 : 2   2x2
  cout << p.value_of(7, 4) << endl;         //   4 : 7 : 3   2x2

  cout << p.value_of(37, 19) << endl;       //         : 18  18x18
  cout << p.value_of(39, 20) << endl;       //         : 19  19x19
  cout << p.value_of(41, 21) << endl;       //         : 20  20x20
    
  return 0;
}
