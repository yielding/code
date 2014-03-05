#include <gmock/gmock.h>
#include <vector>
#include <numeric>

using namespace std;
using namespace testing;

TEST(Numeric, accumulate)
{
  vector<double> v{ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  auto add = [=](double memo, double x) { return memo + x; };
  auto res = accumulate(v.begin(), v.end(), 0.0, add);
  ASSERT_THAT(res, DoubleEq(21.0));
  
}

/*
int main(int argc, char *argv[])
{
  ::testing::InitGoogleMock(&argc, argv);
  return RUN_ALL_TESTS();
}
*/
