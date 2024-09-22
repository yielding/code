#include <vector>
#include <cassert>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int isbn_13(vector<int> const& isbn)
{
  using v::linear_distribute, v::take, v::cycle;

  auto f12  = isbn | take(12);
  auto rcy  = linear_distribute(1, 3, 2) | cycle;
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