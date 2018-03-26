#include "common.h"

TEST(Tuple, ForwardAsTuple)
{
  map<int, string> m;

  m.emplace(forward_as_tuple(10, string(5, 'a')));
  
  ASSERT_THAT(m[10], Eq("aaaaa"));
}

TEST(Tuple, Tie)
{
}

