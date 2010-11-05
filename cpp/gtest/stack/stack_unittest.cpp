#include <gtest/gtest.h>
#include <stack>

class StackTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    m_st.push(1);
    m_st.push(2);
  }

  virtual void TearDown()
  {
  }

protected:
  // stack m_st;
  std::stack<int> m_st;
};

TEST_F(StackTest, Init)
{
  m_st.push(2);
  EXPECT_EQ(m_st.size(), 3);
}

TEST_F(StackTest, Push)
{
  EXPECT_EQ(m_st.size(), 2);
}

TEST_F(StackTest, Top)
{
  EXPECT_EQ(m_st.size(), 2);
  EXPECT_EQ(m_st.top(),  2);

  for (int i=0; i<1000; i++) m_st.push(i);

  EXPECT_EQ(m_st.top(),  999);
}

TEST_F(StackTest, Pop)
{
  m_st.pop();
  EXPECT_EQ(m_st.top(),  1);
  EXPECT_EQ(m_st.size(), 1);
}

TEST_F(StackTest, Empty)
{
  EXPECT_EQ(m_st.empty(), false);
  m_st.pop();
  m_st.pop();
  EXPECT_EQ(m_st.empty(), true);
}
