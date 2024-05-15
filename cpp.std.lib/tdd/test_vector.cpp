#include "common.h"

TEST(Vector, Compact)
{
  vector<int> v{1, 2, 3, 4, 5};

  v.erase(v.begin());
  ASSERT_THAT(v, ElementsAre(2, 3, 4, 5));
  ASSERT_THAT(v.capacity(), Eq(5));

  vector<int> v1(v);
  ASSERT_THAT(v1.capacity(), Eq(4));

  vector<int>(v).swap(v);
  ASSERT_THAT(v.capacity(), Eq(4));
}
