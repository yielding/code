#include <gmock/gmock.h>
#include <set>
#include <vector>
#include <string>

using namespace std;
using namespace testing;

TEST(Set, Difference)
{
  using Strs = vector<string>;

  Strs s1 = {"leech1", "leech2", "leech3" },
       s2 = {"leech1" },
       s3;

  set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
      back_inserter(s3));

  ASSERT_THAT(s3, Eq(vector<string>{"leech2", "leech3"}));
  ASSERT_THAT(s3, ElementsAre("leech2", "leech3"));
}
