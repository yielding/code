#include <iostream>
#include <string>
#include <map>
#include <optional>
#include <stack>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

static map<string, function<int(int, int)>> opMap 
{
  { "+", plus{} },
  { "-", minus{} },
  { "*", multiplies{} }
};

auto to_int(string const& s) 
{
  int i;
  istringstream iss(s);
  iss >> i;

  return !iss.fail() 
    ? make_optional(i)
    : nullopt;
}

auto update(stack<int> sk, string const& s)
{
  if (opMap.contains(s) && sk.size() > 1)
  {
    auto b = sk.top(); sk.pop();
    auto a = sk.top(); sk.pop();
    sk.push(opMap[s](a, b));
  }
  else if (auto i_op = to_int(s))
  {
    sk.push(*i_op);
  }

  return sk;
}

int main(int agc, char* agv[])
{
  auto ss = stringstream{"9 2 1 + 2 * -"};
  auto st = g::accumulate(g::istream<string>(ss),
      stack<int>{},
      update);

  if (!empty(st))
    cout << st.top() << endl;

  return 0;
}