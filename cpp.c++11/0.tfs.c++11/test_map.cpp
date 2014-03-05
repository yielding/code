#include <gmock/gmock.h>
#include <map>
#include <string>

using namespace std;
using namespace testing;

TEST(Map, Memvership)
{
  map<string, unsigned> heights {
    {"yielding", 181},
    {"leeks", 178}
  };

  ASSERT_THAT(heights["yielding"], Eq(181));
  ASSERT_THAT(heights["leeks"],    Eq(178));
}
