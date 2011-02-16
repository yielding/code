#include <gtest/gtest.h>

#include <iostream>
#include <algorithm>
#include <cstring>
#include <string>

using namespace std;

class MemmoveTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    // m_s = "abcdefghijklmnopqrstuvwxyz";
  }

  virtual void TearDown()
  {
  }

  string m_s;
};


TEST_F(MemmoveTest, Memmove)
{
  for (size_t i=0; i<m_s.length(); ++i)
  {
    char c = m_s[0];
    memmove(&m_s[0], &m_s[1], m_s.length());
    m_s[m_s.length()-1] = c;
    cout << m_s << endl;
  }
}

TEST_F(MemmoveTest, Rotate)
{
  ASSERT_TRUE(true);
  for (size_t i=0; i<m_s.length(); ++i)
  {
    rotate(m_s.begin(), m_s.begin()+1, m_s.end());
    cout << m_s << endl;
  }
}
