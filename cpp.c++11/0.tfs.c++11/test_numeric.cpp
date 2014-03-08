#include "common.h"

#include <numeric>

struct adder
{
  template <typename T> 
  T operator()(T memo, T elem) { return memo + elem; }
  // auto add = [=](double memo, double x) { return memo + x; };
};

TEST(Numeric, accumulate)
{
  vector<double> v{ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  auto res = accumulate(v.begin(), v.end(), 0.0, adder());
  ASSERT_THAT(res, DoubleEq(21.0));
}

TEST(Numeric, iota)
{
  int a[5] = { 0 };
  iota(a, a+5, 10);
  ASSERT_THAT(a, ElementsAre(10, 11, 12, 13, 14));
  auto sum = accumulate(a, a+5, 0,  adder());
}
