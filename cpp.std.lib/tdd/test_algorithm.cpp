#include "common.h"

#include <numeric>
#include <array>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>

using namespace boost::phoenix::arg_names;

TEST(Algorithm, Assign)
{
  vector<int> v1(10); iota(v1.begin(), v1.end(), 1);
  ASSERT_THAT(v1, ElementsAre(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

  vector<int> v2;
  v2.assign(v1.begin() + 5, v1.end());
  ASSERT_THAT(v2, ElementsAre(6, 7, 8, 9, 10));
}

TEST(Algorithm, ReplaceIf1)
{
  array<int, 5> a; iota(a.begin(), a.end(), 1);
  ASSERT_THAT(a, ElementsAre(1, 2, 3, 4, 5));

  replace_if(a.begin(), a.end(), [](int a) { return a % 2 == 0; }, 100);
  ASSERT_THAT(a, ElementsAre(1, 100, 3, 100, 5));
}

TEST(Algorithm, ReplaceIf2)
{
  array<int, 5> a; iota(a.begin(), a.end(), 1);
  ASSERT_THAT(a, ElementsAre(1, 2, 3, 4, 5));

  replace_if(a.begin(), a.end(), arg1 % 2 == 0, 100);
  ASSERT_THAT(a, ElementsAre(1, 100, 3, 100, 5));
}

TEST(Algorithm, SwapRange)
{
  vector<int> v{1, 2, 3, 4, 5};
  list<int> l{-1, -2, -3, -4, -5};

  swap_ranges(v.begin(), v.begin() + 3, l.begin());
  ASSERT_THAT(l, ElementsAre(1, 2, 3, -4, -5));
}

TEST(Algorithm, PartitionCopy)
{
  int arr[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  int true_arr[5] = { 0 };
  int fals_arr[5] = { 0 };

  partition_copy(begin(arr), end(arr),
                 begin(true_arr),
                 begin(fals_arr),
                 [](int i) { return i > 5; });

  ASSERT_THAT(true_arr, ElementsAre(6, 7, 8, 9, 10));
  ASSERT_THAT(fals_arr, ElementsAre(1, 2, 3, 4, 5));
}
