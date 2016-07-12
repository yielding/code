#include <iostream>
#include <vector>
#include <map>

using namespace std;

auto count_no(int no) -> map<int, int>
{
  std::map<int, int> result;

  while (no > 0)
  {
    auto value = no % 10;

    no = no / 10;

    result[value]++;
  }

  return result;
}

auto count_no_range(vector<int> const& nos) -> map<int, int>
{
  std::map<int, int> result;

  for (auto no: nos)
  {
    auto res = count_no(no);
    for (auto pair: res)
      result[pair.first] += pair.second;
  }

  return result;
}

void pr(map<int, int> r)
{
  for (auto p: r)
    cout << p.first << ": " << p.second << endl;
}

int main(int argc, char *argv[])
{
  vector<int> nos{100, 200};

  auto r0 = count_no_range(nos);

  pr(r0);

  return 0;
}
