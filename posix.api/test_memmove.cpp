#include <gtest/gtest.h>

#include <iostream>
#include <algorithm>
#include <cstring>
#include <string>

using namespace std;

class MemmoveTest: public testing::Test
{
public:
  enum { BUF_SIZE = 40960 };

protected:
  virtual void SetUp()
  {
    m_s1 = "abcdefghijklmnopqrstuvwxyz";
    m_s2 = m_s1;

    for (int i=0; i<BUF_SIZE; ++i) m_buffer[i] = 0;
  }

  virtual void TearDown()
  {
  }

  string m_s1, m_s2;
  char   m_buffer[BUF_SIZE];
};

TEST_F(MemmoveTest, Memmove)
{
  for (size_t i=0; i<m_s1.length(); ++i)
  {
    char c = m_s1[0];
    memmove(&m_s1[0], &m_s1[1], m_s1.length());
    m_s1[m_s1.length()-1] = c;
    rotate(m_s2.begin(), m_s2.begin()+1, m_s2.end());
    ASSERT_TRUE(m_s1 == m_s2);
  }
}

TEST_F(MemmoveTest, MemmoveSpeed)
{
  for (int i=0; i<BUF_SIZE; ++i) 
  {
    char c = m_buffer[0];
    memmove(m_buffer, m_buffer+1, BUF_SIZE);
    m_buffer[BUF_SIZE-1] = c;
  }
}

TEST_F(MemmoveTest, RotateSpeed)
{
  for (int i=0; i<BUF_SIZE; ++i) 
    rotate(m_buffer, m_buffer + 1, m_buffer + BUF_SIZE);
}
