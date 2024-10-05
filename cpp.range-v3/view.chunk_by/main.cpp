#include <iostream>
#include <map>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

void group_according_to_predicate()
{
  auto const s = string{"12ab345de"};
  auto is_digit = [](auto x, auto y) { return isdigit(x) == isdigit(y); };
  auto rng = s | v::chunk_by(is_digit); // [[1,2],[a,b],[3,4,5],[d,e]]
  cout << v::all(rng) << endl;
}

void group_positivie_negative()
{
  auto const v = vector{ -8, -4, -2, 1, 2, 4 };
  auto gt_zero = [](auto x, auto y) { return x * y > 0; };
  auto rng = v | v::chunk_by(gt_zero); // [[-8,-4,-2],[1,2,4]]
  cout << v::all(rng) << endl;
}

int main(int argc, char* argv[])
{
  group_according_to_predicate();
  group_positivie_negative();

  return 0;
}
