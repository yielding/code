#include <gtest/gtest.h>
#include <worker.h>

#include <iostream>

using namespace std;
using namespace core;
using namespace boost;
//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class HttpParserTest: public testing::Test
{
public:
  typedef sys::dylib<core::worker> plugin_t;

public:

  HttpParserTest()
  {
  }

  ~HelloWorkerTest()
  {
  }

protected:
  virtual void SetUp()
  {
  }

  virtual void TearDown()
  {
  }

protected:
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
TEST_F(HelloWorkerTest, Dylib)
{
  EXPECT_EQ(m_load_ok, true);
}

TEST_F(HelloWorkerTest, Execute)
{
  if (m_load_ok)
  {
    parameter_list in, out;
    m_worker->execute(core::PCL_USER_DEF, "hello", in, out);
    char* res =  out.get_field("hello");
    EXPECT_EQ(strcmp(res, "world"), 0);
  }
}
