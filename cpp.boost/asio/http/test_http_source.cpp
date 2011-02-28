#include "http_source.h"

#include <boost/iostreams/filtering_stream.hpp>
#include <fstream>
#include <gtest/gtest.h>

using namespace std;
namespace io = boost::iostreams;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HTTPSourceTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    m_source = new HTTPSource();
  }

  virtual void TearDown()
  {
    delete m_source;
  }

  HTTPSource* m_source;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
TEST_F(HTTPSourceTest, FalseHandshake)
{
  EXPECT_TRUE(!m_source->handshake());
}

TEST_F(HTTPSourceTest, HandshakeAndGetData)
{
  ifstream in("LICENSE_1_0.txt");
  EXPECT_TRUE(in.is_open());

  EXPECT_TRUE(m_source->handshake("www.boost.org", "/LICENSE_1_0.txt", 60));
  while (true) 
  {
    int const SIZE = 4096;
    char nbuf[SIZE+1] = { 0 };
    streamsize read = m_source->read(nbuf, SIZE);
    if (read <= 0)
      break;

    char fbuf[SIZE+1] = { 0 };
    in.read(fbuf, read);
    ASSERT_EQ(in.gcount(), read);
    for (int i=0; i<read; ++i)
      ASSERT_EQ(nbuf[i], fbuf[i]);
  }
}

TEST_F(HTTPSourceTest, GetIndexFile)
{
  EXPECT_TRUE(m_source->handshake("www.boost.org", "/", 60));
  while (true) 
  {
    int const SIZE = 4096;
    char buf[SIZE+1] = { 0 };
    streamsize n = m_source->read(buf, SIZE);
    if (n <= 0)
      break;

    EXPECT_TRUE(n>0);
  }
}

TEST_F(HTTPSourceTest, ReadAll)
{
  EXPECT_TRUE(m_source->handshake("www.boost.org", "/LICENSE_1_0.txt", 60));

  ASSERT_EQ(1338, m_source->read_all());
  vector<char>& buffer = m_source->read_buffer();

  ifstream in("LICENSE_1_0.txt");
  EXPECT_TRUE(in.is_open());
  char buf[4096] = { 0 };
  in.read(buf, 4096);
  ASSERT_EQ(in.gcount(), 1338);
  ASSERT_EQ(memcmp(buf, &buffer[0], 1338), 0);
}

TEST_F(HTTPSourceTest, Timeout)
{
  EXPECT_TRUE(!m_source->handshake("www.boost.org", "/", 1));
}

TEST_F(HTTPSourceTest, SaveToFile)
{
  ofstream out("www.boost.org-index.html");
  EXPECT_TRUE(m_source->handshake("www.boost.org", "/", 60));

  while (true) 
  {
    int const SIZE = 4096;
    char buf[SIZE+1] = { 0 };
    streamsize n = m_source->read(buf, SIZE);
    if (n <= 0)
      break;

    out.write(buf, n);
  }
}
//

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
