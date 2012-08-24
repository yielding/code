#include <iostream>
#include <vector>
#include <string>

using namespace std;

class Pascal
{
public:
  auto value_of(int x, int y) -> int;
  auto values_of(int row)     -> vector<int>;
  auto line_of(int row)       -> string;
  auto lines_upto(int row)    -> string;
};

auto Pascal::value_of(int x, int y) -> int
{
  if (x < 1 || y < 1 || y > x)
    return 0;

  if (x == 1)
    return 1;

  return value_of(x-1, y-1) + value_of(x-1, y);
}

auto Pascal::values_of(int row) -> vector<int>
{
  vector<int> v;

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
    sprintf (buf, "%6d", value_of(row, i));
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

  cout << p.lines_upto(7);

  return 0;
}
