#include <gtest/gtest.h>

#include <boost/property_tree/ptree.hpp>
#include <iostream>

using namespace std;

class PTreePathTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    m_start_time = time(NULL);
  }

  virtual void TearDown()
  {
    time_t const end_time = time(NULL);
    EXPECT_TRUE(end_time - m_start_time <= 5) << "The Test took too long.";
  }

  time_t m_start_time;

protected:
  boost::ptree m_ptree;
};

TEST_F(PTreePathTest, True)
{
  ASSERT_EQ(true, true);
}

TEST_F(PTreePathTest, PathType)
{
  typedef ptree::path_type path;

  ASSERT_EQ(true, true);
}
