#include "progressable.h"

#include <gtest/gtest.h>
#include <iostream>
#include <algorithm>
#include <boost/bind.hpp>

using namespace std;

class DataSource: public progressable
{
public:
  DataSource(int64_t size=1024)
    : m_size(size), m_left(size), m_pos(0), m_last_pos(0)
  {}

  // loopback
  void progress(int64_t pos)
  {
     m_last_pos = pos;
  }

  int64_t last_pos()
  {
    return m_last_pos;
  }

  int64_t position()
  {
    return m_pos;
  }

  int read(char* buf, size_t n)
  {
    int64_t read = std::min<int64_t>(m_size-m_pos, n);
    if (read <= 0) return 0;

    m_pos += n;
    notify_progress(m_pos);

    return read;
  }

private:
  int64_t m_pos, m_last_pos;
  int64_t m_size, m_left;
};

class ProgressableTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
  }

  virtual void TearDown()
  {
  }
};

TEST_F(ProgressableTest, Read)
{
  DataSource source(31);

  char buf[10];
  ASSERT_EQ(source.read(buf, 10), 10);
  
  ASSERT_EQ(source.read(buf, 10), 10);
  ASSERT_EQ(source.read(buf, 10), 10);
  ASSERT_EQ(source.read(buf, 10), 1);
  ASSERT_EQ(source.read(buf, 10), 0);
  ASSERT_EQ(source.read(buf, 10), 0);
}

TEST_F(ProgressableTest, ProgressConnected)
{
  DataSource source(31);

  source.progress_notifier(boost::bind(&DataSource::progress, &source, ::_1));

  char buf[10];
  ASSERT_EQ(source.read(buf, 10), 10); ASSERT_EQ(source.last_pos(), source.position());
  ASSERT_EQ(source.read(buf, 10), 10); ASSERT_EQ(source.last_pos(), source.position());
  ASSERT_EQ(source.read(buf, 10), 10); ASSERT_EQ(source.last_pos(), source.position());
  ASSERT_EQ(source.read(buf, 10), 1);  ASSERT_EQ(source.last_pos(), source.position());
  ASSERT_EQ(source.read(buf, 10), 0);  ASSERT_EQ(source.last_pos(), source.position());
}

TEST_F(ProgressableTest, ProgressNotConnected)
{
  DataSource source(31);

  char buf[10];

  ASSERT_EQ(source.read(buf, 10), 10); ASSERT_EQ(source.last_pos(), 0);
  ASSERT_EQ(source.read(buf, 10), 10); ASSERT_EQ(source.last_pos(), 0);
  ASSERT_EQ(source.read(buf, 10), 10); ASSERT_EQ(source.last_pos(), 0);
  ASSERT_EQ(source.read(buf, 10),  1); ASSERT_EQ(source.last_pos(), 0);
  ASSERT_EQ(source.read(buf, 10),  0); ASSERT_EQ(source.last_pos(), 0);
}
