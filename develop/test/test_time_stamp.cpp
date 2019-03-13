#include <gtest/gtest.h>
#include "sys/time_stamp.h"

class TimeStampTest: public testing::Test 
{
public:
  TimeStampTest() {}

protected:
  virtual void SetUp() 
  {}

  virtual void TearDown() {}
};

TEST_F(TimeStampTest, YearMonthDay)
{
  int y = 2019;
  int m = 3;
  int d = 13;
  int h = 15;
  int M = 33;
  int s = 55;
  int us = 123456;
  sys::TimeStamp ts(y, m, d, h, M, s, us);

  EXPECT_EQ(ts.to_s(), "2019-03-13 15:33:55.123456");
}

TEST_F(TimeStampTest, TimeT)
{
  sys::TimeStamp ts(time_t(1552287917));
  EXPECT_EQ(ts.to_s(), "2019-03-11 07:05:17.000000");
}
