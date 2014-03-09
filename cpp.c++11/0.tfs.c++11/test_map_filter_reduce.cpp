#include "common.h"

#include <numeric>

struct adder
{
  template <typename T> 
  T operator()(T memo, T elem) { return memo + elem; }
};

TEST(MapFilterReduce, Reduce1)
{
  vector<double> v{ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  auto res = accumulate(v.begin(), v.end(), 0.0, adder());
  ASSERT_THAT(res, DoubleEq(21.0));
}

TEST(MapFilterReduce, Reduce2)
{
  int a[5]{ 0 };
  iota(a, a+5, 10);
  ASSERT_THAT(a, ElementsAre(10, 11, 12, 13, 14));
  auto sum = accumulate(a, a+5, 0,  adder());
}

TEST(MapFilterReduce, MapByForEach)
{
  vector<string> v{"one", "two", "three"};
  for_each(v.begin(), v.end(), [](string& s) { s+= "-l"; });

  ASSERT_THAT(v, ElementsAre("one-l", "two-l", "three-l"));
}

TEST(MapFilterReduce, MapByTransform1)
{
  vector<string> v1{"one", "two", "three"}, v2;

  transform(v1.begin(), v1.end(), v1.begin(),
      [](string s) { return s + "-l"; }
      );

  ASSERT_THAT(v1, ElementsAre("one-l", "two-l", "three-l"));
}

TEST(MapFilterReduce, MapByTransform2)
{
  vector<string> v1{"one", "two", "three"}, v2;

  transform(v1.begin(), v1.end(), back_inserter(v2),
      [](string s) { return s + "-l"; }
      );

  ASSERT_THAT(v2, ElementsAre("one-l", "two-l", "three-l"));
}

