#include "long_runnable.h"

#include <boost/bind.hpp>
#include <gtest/gtest.h>
#include <algorithm>
#include <iostream>

using namespace std;

class LongTermTask: public long_runnable<bool, int>
{
public:
  LongTermTask() {}

  int Execute()
  {
    if (should_stop()) return 0;
    if (notify_result(m_arg)) return 1;

    return 2;
  }

  int m_arg;  // if m_arg is struct then every data can be described.
};

class TimedTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    m_start_time = time(NULL);
  }

  virtual void TearDown()
  {
    time_t const end_time = time(NULL);
    EXPECT_TRUE(end_time - m_start_time < 10) << "The Test took too long.";
  }

private:
  time_t m_start_time;
};

class LongTermRunnableTest: public TimedTest
{
protected:
  virtual void SetUp()
  {
    TimedTest::SetUp();
  }

  virtual void TearDown()
  {
    TimedTest::TearDown();
  }

  LongTermTask m_t0;

public:
  bool should_terminate_true()  { return true;   }
  bool should_terminate_false() { return false;  }
  bool read_result0(int i)      { return i == 1; }
};

TEST_F(LongTermRunnableTest, Task0ShouldStop)
{
  ASSERT_EQ(m_t0.Execute(), 2);
  m_t0.stop_checker(boost::bind(&LongTermRunnableTest::should_terminate_true, this));
  ASSERT_EQ(m_t0.Execute(), 0);
}

TEST_F(LongTermRunnableTest, Task0Notify)
{
  ASSERT_EQ(m_t0.Execute(), 2);
  m_t0.stop_checker(boost::bind(&LongTermRunnableTest::should_terminate_false, this));
  ASSERT_EQ(m_t0.Execute(), 2);
  m_t0.result_notifier(boost::bind(&LongTermRunnableTest::read_result0, this, ::_1));
  m_t0.m_arg = 1;
  ASSERT_EQ(m_t0.Execute(), 1);
  m_t0.m_arg = 2;
  ASSERT_EQ(m_t0.Execute(), 2);
}
