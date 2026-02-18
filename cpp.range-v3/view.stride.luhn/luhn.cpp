#include <iostream>
#include <cassert>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

void basics()
{
  auto r0 = v::iota(1, 13) | v::stride(3); // [1, 4, 7, 10]
  cout << v::all(r0) << endl;

  auto r1 = v::iota(1, 13) | v::reverse | v::stride(3); // [12, 9, 6, 3]
  cout << v::all(r1) << endl;
}

string luhn_algorithm(vector<int> card_nos)
{
  using v::stride, v::drop, v::transform, v::concat, g::accumulate;

  auto es = card_nos | stride(2);
  auto os = card_nos | drop(1) | stride(2);
  auto trans_es = es | transform([](int x) {
      auto y = x * 2;
      return y < 9 ? y : y - 9; });

  auto sum = accumulate(concat(os, trans_es),  0);

  return sum % 10 == 0 ? "ok" : "wrong";
}

int main(int argc,  char* argv[])
{
  basics();

  auto no = { 4, 5, 3, 9, 1, 4, 8, 8, 0, 3, 4, 3, 6, 4, 6, 7 };
  assert(luhn_algorithm(no) == "ok");

  return 0;
}