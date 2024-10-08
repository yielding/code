#include <iostream>
#include <print>
#include <algorithm>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

void basics()
{
  // predicate가 true이면 이전 그룹으로 전부
  // predicate가 false이면 first는 이전, second는 이후
  auto const v = {1, 1, 2, 2, 3, 3, 3};
  auto chunks = v | v::chunk_by(equal_to{});

  cout << v::all(chunks) << endl;
}

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
  auto rng = v | v::chunk_by(gt_zero); // [[-8,-4,-2], [1,2,4]]
                                       
  cout << v::all(rng) << endl;
}

int main(int argc, char* argv[])
{
  basics();
  group_according_to_predicate();
  group_positivie_negative();

  return 0;
}
