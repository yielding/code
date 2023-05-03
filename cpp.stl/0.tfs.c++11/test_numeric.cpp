#include "common.h"

#include <numeric>

TEST(Numeric, iota)
{
  int a[5] = { 0 };
  iota(a, a+5, 10);
  ASSERT_THAT(a, ElementsAre(10, 11, 12, 13, 14));
  auto add = [=](double memo, double x) { return memo + x; };
  auto sum = accumulate(a, a+5, 0, add);
}
