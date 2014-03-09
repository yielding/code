#include "common.h"

#include <numeric>

TEST(Algorithm, assign)
{
  vector<int> v1(10); iota(v1.begin(), v1.end(), 1);
  ASSERT_THAT(v1, ElementsAre(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

  vector<int> v2;
  v2.assign(v1.begin() + 5, v1.end());
  ASSERT_THAT(v2, ElementsAre(6, 7, 8, 9, 10));
}
