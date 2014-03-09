#include "common.h"

TEST(Numeric, MapByForEach)
{
  vector<string> v{"one", "two", "three"};
  for_each(v.begin(), v.end(), [](string& s) { s+= "-l"; });

  ASSERT_THAT(v, ElementsAre("one-l", "two-l", "three-l"));
}

TEST(Numeric, MapByTransform)
{
  vector<string> v1{"one", "two", "three"}, v2;

  transform(v1.begin(), v1.end(), back_inserter(v2),
      [](string s) { return s + "-l"; }
      );

  ASSERT_THAT(v2, ElementsAre("one-l", "two-l", "three-l"));
}
