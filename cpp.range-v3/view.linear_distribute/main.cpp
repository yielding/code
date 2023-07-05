#include <iostream>
#include <vector>
#include <cassert>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;
using namespace std;

int isbn_13(vector<int> const& isbn)
{
  auto f12  = isbn | v::take(12);
  auto rcy  = v::linear_distribute(1, 3, 2) | v::cycle;
  auto conv = g::inner_product(f12, rcy, 0);
  auto rem = conv % 10;

  return rem == 0 ? 0 : 10 - rem;
}

int main(int argc, char* argv[])
{
  auto isbn = vector{ 9, 7, 8, 8, 1, 6, 1, 9, 7, 2, 7, 1, 2 };

  auto r = isbn_13(isbn);
  assert(r == 8);

  auto actual = *isbn.rbegin();
  assert(r != actual);
  
  return 0;
}