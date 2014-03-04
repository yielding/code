#include <gmock/gmock.h>
#include <functional>
#include <string>

using namespace std;
using namespace testing;

void operator*(int times, function<void(int)> f)
{
  for (int i=0; i<times; i++) f(i);
}

TEST(Lambda, OperatorTimes)
{
  auto total = 0;
  5 * [&](int i) { total += i; };

  ASSERT_THAT(total, Eq(0 + 1 + 2 + 3 + 4));
}
