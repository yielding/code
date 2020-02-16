#include <gtest/gtest.h>
#include <string>
#include <iostream>

using namespace std;

class SampleTest: public testing::Test 
{
public:
  SampleTest()
  {}

protected:
  virtual void SetUp() {
  }

  virtual void TearDown() {
  }
};

TEST(SampleTest, AlwaysTrue)
{
  EXPECT_EQ(1, 1);
}
