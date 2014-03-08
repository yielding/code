#include <gmock/gmock.h>
#include <map>
#include <string>

using namespace std;
using namespace testing;

TEST(Map, MembershipTest)
{
  map<string, unsigned> heights {
    {"yielding", 181},
    {"leeks", 178}
  };

  ASSERT_THAT(heights["yielding"], Eq(181));
  ASSERT_THAT(heights["leeks"],    Eq(178));
}

TEST(Map, Insert)
{
  map<int, string> m;

  m[1]="January";
  auto inserted = m.insert(make_pair(2, "February")).second;
  ASSERT_TRUE(inserted);

  inserted = m.insert(make_pair(2, "March")).second;
  ASSERT_FALSE(inserted);

  inserted = m.insert(make_pair(3, "March")).second;
  ASSERT_TRUE(inserted);
}
