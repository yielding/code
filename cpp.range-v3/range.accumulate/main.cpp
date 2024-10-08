#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  auto const v = {1, 7, 5, 6, 9, 3, 5, 8, 4};
  // g::accumulate(v, 0) == 1 + 7 + 5 + 6 + ..
  auto val = g::accumulate(v, 0LL, [](auto a, auto b) { return a*10 + b;}); // 175693584
  cout << val << endl;

  return 0;
}
