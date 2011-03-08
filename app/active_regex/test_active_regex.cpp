#include "active_regex.h"
// #include "filebase_source.h"
#include "filebase_device.h"
#include "asio_threadpool.h"

#include <gtest/gtest.h>

#include <boost/iostreams/filtering_stream.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/iostreams/stream.hpp>  // io::stream
#include <boost/iostreams/detail/ios.hpp>
#include <boost/format.hpp>
#include <fstream>

using namespace std;
namespace io = boost::iostreams;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class ActiveRegexEnvironment: public ::testing::Environment
{
public:
  ActiveRegexEnvironment()
  {}

  virtual ~ActiveRegexEnvironment()
  {}

  virtual void SetUp() 
  {
    m_pool = new sys::threadpool;
    m_pool->start();
  }

  virtual void TearDown()
  {
    sleep(1);
    delete m_pool;
  }

  sys::threadpool* m_pool;
};

ActiveRegexEnvironment* const ar_env 
  = (ActiveRegexEnvironment* const)::testing::AddGlobalTestEnvironment(new ActiveRegexEnvironment);

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class ActiveRegexTest: public testing::Test
{
protected:
  virtual void SetUp() 
  {
    m_start_time = time(NULL);
  }

  virtual void TearDown() 
  {
    time_t const end_time = time(NULL);
    EXPECT_TRUE(end_time - m_start_time <= 1000) << "The test took too long.";
  }

public:
  void test_file(string const& size, int64_t buffer_size, int64_t end_offset);
  void test_threadpool(match_result const& m);

public:
  ActiveRegex m_regex;

private:
  void thread_func(match_result const& m);

private:
  time_t m_start_time;
};

void ActiveRegexTest::test_file(string const& size, int64_t buffer_size, int64_t end_offset)
{
  using namespace boost; 
  std::fstream in;

  string const& file_name = str(format("test_%s.bin") % size);

  in.open(file_name.c_str(), ios_base::binary | ios_base::in);
  m_regex.buffer_size(buffer_size);
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 0);
  ASSERT_EQ(r[0].length, 2);
  ASSERT_EQ(r[1].offset, end_offset);
  ASSERT_EQ(r[1].length, 2);

  in.close();
}

void ActiveRegexTest::test_threadpool(match_result const& m)
{
  ar_env->m_pool->post(boost::bind(&ActiveRegexTest::thread_func, this, m));
}

void ActiveRegexTest::thread_func(match_result const& m)
{
  EXPECT_TRUE(true);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
/* Memory Stream begin */
TEST_F(ActiveRegexTest, Search0)
{
  string text = "123456789012345678bcd23456789012345678901234567890";
          //     |        |        ^|         |         |         |
          //     0        10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_NE(m_regex.search(boost::regex("abc"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.empty(), true);
}

TEST_F(ActiveRegexTest, Search1)
{
  string text = "123abc789012345678bcd23456789012345678901234567890";
          //     |  ^     |        ^|         |         |         |
          //     0        10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("abc"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 3);
  ASSERT_EQ(r[0].length, 3);
}

TEST_F(ActiveRegexTest, Search2)
{
  string text = "123abc789012345678bcd23456789012345678901234abc890";
          //     |  ^      |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("abc"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 3);
  ASSERT_EQ(r[0].length, 3);
  ASSERT_EQ(r[1].offset, 44);
  ASSERT_EQ(r[1].length, 3);
}

TEST_F(ActiveRegexTest, SearchPartial1)
{
  string text = "123456789bc2345678bcd23456789012345678901234abc890";
          //     |         |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 18);
  ASSERT_EQ(r[0].length, 3);
}

TEST_F(ActiveRegexTest, SearchPartial1_1)
{
  string text = "123456789bc23bcd78bcd23456789012345678901234abc890";
  //     |         |       ^ |         |         |         |
  //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 13);
  ASSERT_EQ(r[0].length, 3);
  ASSERT_EQ(r[1].offset, 18);
  ASSERT_EQ(r[1].length, 3);
}

TEST_F(ActiveRegexTest, SearchPartial2)
{
  string text = "123456789012345678bcd23456789012345678901234abc890";
          //     |         |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 18);
  ASSERT_EQ(r[0].length, 3);
}

TEST_F(ActiveRegexTest, SearchOr)
{
  string text = "123456789012345678bcd23456789012345678901234abc890";
          //     |         |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("abc|bcd"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 18);
  ASSERT_EQ(r[0].length, 3);
  ASSERT_EQ(r[1].offset, 44);
  ASSERT_EQ(r[1].length, 3);
  ASSERT_EQ(text.substr(r[0].offset, r[0].length), "bcd");
  ASSERT_EQ(text.substr(r[1].offset, r[1].length), "abc");
}

TEST_F(ActiveRegexTest, SearchOr2)
{
  string text = "123456789012345678bcd23456789012345678901234acd890";
          //     |         |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("(a|b)cd"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 18);
  ASSERT_EQ(r[0].length, 3);
  ASSERT_EQ(r[1].offset, 44);
  ASSERT_EQ(r[1].length, 3);
  ASSERT_EQ(text.substr(r[0].offset, r[0].length), "bcd");
  ASSERT_EQ(text.substr(r[1].offset, r[1].length), "acd");
}

TEST_F(ActiveRegexTest, SearchGroup)
{
  string text = "123456789012345678bcd23456789012345678901234acd890";
          //     |         |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("(acd)"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 44);
  ASSERT_EQ(r[0].length, 3);
  ASSERT_EQ(text.substr(r[0].offset, r[0].length), "acd");
}

string escape(string const& key)
{
  const boost::regex esc("[\\^\\.\\$\\|\\(\\)\\[\\]\\*\\+\\?\\/\\\\]");
  const std::string replace("\\\\\\1&");
  return regex_replace(key, esc, replace, boost::match_default | boost::format_sed);
}

TEST_F(ActiveRegexTest, Escape)
{
  string text = "0123456789012345678(bcd)456789^1234567890123456789";
  string key1 = "(bcd)"; key1 = escape(key1); 
  string key2 = "^";     key2 = escape(key2); 

  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);

  ASSERT_EQ(m_regex.search(boost::regex(key1 + "|" + key2), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 19);
  ASSERT_EQ(r[0].length, 5);
  ASSERT_EQ(r[1].offset, 30);
  ASSERT_EQ(r[1].length, 1);
  ASSERT_EQ(text.substr(r[0].offset, r[0].length), "(bcd)");
  ASSERT_EQ(text.substr(r[1].offset, r[1].length), "^");
}

TEST_F(ActiveRegexTest, SearchPartial3)
{
  string text = "123abc789012345678bcd23456789012345678xxxxx4abc890";
          //     |  ^      |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("x{5}"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 38);
  ASSERT_EQ(r[0].length, 5);
}

TEST_F(ActiveRegexTest, SearchPartial4)
{
  string text = "xx3456789x123456789x123456789x123456789x12345678xx";
          //     |  ^      |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  m_regex.buffer_size(10);
  ASSERT_EQ(m_regex.search(boost::regex("xx"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(text.length(), 50);
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 0);
  ASSERT_EQ(r[0].length, 2);
  ASSERT_EQ(r[1].offset, 48);
  ASSERT_EQ(r[1].length, 2);
}

TEST_F(ActiveRegexTest, BinarySearch)
{
  string text = "12345678901234567890123456789012345678901234567890";
          //     |         |         |         |         |         |
          //     0         10        20        30        40        50
  text[18] = 0xff;
  text[19] = 0xff;
  text[20] = 0xff;
  text[21] = 0xfd;
  io::filtering_istream in(boost::make_iterator_range(text));
  for (int size=16; size<=4096; size+=16)
  {
    m_regex.buffer_size(size);
    ASSERT_EQ(m_regex.search(boost::regex("\xff{3}\xfd"), in), true);
    matches const& r = m_regex.result();
    ASSERT_EQ(r.size(), 1);
    ASSERT_EQ(r[0].offset, 18);
    ASSERT_EQ(r[0].length, 4);

    ASSERT_EQ(m_regex.search(boost::regex("\xff\xff\xff\xfd"), in), true);
    matches const& s = m_regex.result();
    ASSERT_EQ(s.size(), 1);
    ASSERT_EQ(s[0].offset, 18);
    ASSERT_EQ(s[0].length, 4);
  }
}

/* FileBase Begin */
TEST_F(ActiveRegexTest, FileBase1)
{
  FileBase* fb = new FileBase;
  FileBaseDevice fbs(fb);
  io::stream<FileBaseDevice> in(fbs);

  m_regex.buffer_size(1024*16);
  in.seekg(0, ios_base::beg);
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 0);
  ASSERT_EQ(r[0].length, 2);
  ASSERT_EQ(r[1].offset, 1024*1024-2);
  ASSERT_EQ(r[1].length, 2);

  delete fb;
}

TEST_F(ActiveRegexTest, ThreadPoolTest)
{
  std::fstream in;
  in.open("test_1g.bin", ios_base::binary | ios_base::in);
  m_regex.buffer_size(1024*16);
  m_regex.result_notifier(boost::bind(&ActiveRegexTest::test_threadpool, this, ::_1));
  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in, true), true);
}

//TEST_F(ActiveRegexTest, ThreadPoolTestr2)
//{
//  std::fstream in;
//  in.open("test_1g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*64);
//  m_regex.result_notifier(boost::bind(&ActiveRegexTest::test_threadpool, this, ::_1));
//  ASSERT_EQ(m_regex.search(boost::regex("\xf2\xf3"), in, true), true);
//}
//
//TEST_F(ActiveRegexTest, ThreadPoolStoppableTest)
//{
//  std::fstream in;
//  in.open("test_1g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*64);
//  m_regex.result_notifier(boost::bind(&ActiveRegexTest::test_threadpool, this, ::_1));
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in, true), true);
//}

bool should_stop()
{
  static int count = 0;
  return ++count == 2;
}

TEST_F(ActiveRegexTest, ThreadPoolStoppableTest)
{
  std::fstream in;
  in.open("test_1g.bin", ios_base::binary | ios_base::in);
  m_regex.buffer_size(1024*16);
  m_regex.result_notifier(boost::bind(&ActiveRegexTest::test_threadpool, this, ::_1));
  m_regex.stop_checker(boost::bind(&should_stop));
  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in, true), false);
  ASSERT_EQ(m_regex.result().size(), 0);
}

///* FILE begin */
//
//TEST_F(ActiveRegexTest, LargeFile1M_PerLine)
//{
//  std::fstream in;
//  in.open("test_1m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xf2f3"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 65536);
//  in.close();
//}
//
//TEST_F(ActiveRegexTest, LargeFile1M_16K_Buffer)
//{
//  test_file("1m", 1024*16, 1024*1024-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile2M_16K_Buffer)
//{
//  test_file("2m", 1024*16, 1024*1024*2-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile3M_16K_Buffer)
//{
//  test_file("3m", 1024*16, 1024*1024*3-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile10M_16K_Buffer)
//{
//  test_file("10m", 1024*16, 1024*1024*10-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile100M_16K_Buffer)
//{
//  test_file("100m", 1024*16, 1024*1024*100-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile200M_4K_Buffer)
//{
//  test_file("200m", 1024*16, 1024*1024*200-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile500M_16K_Buffer)
//{
//  test_file("500m", 1024*16, 1024*1024*500-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile1G_16K_Buffer)
//{
//  test_file("1g", 1024*16, 1024*1024*1024-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile2G_16K_Buffer)
//{
//  test_file("2g", 1024*16, 1024*1024*1024*2-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile1G_32K_Buffer)
//{
//  test_file("1g", 1024*32, 1024*1024*1024-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile1G_64K_Buffer)
//{
//  test_file("1g", 1024*64, 1024*1024*1024-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile512K16G__Buffer)
//{
//  test_file("16g", 1024*512, 1024LL*1024*1024*16-2);
//}
//
//TEST_F(ActiveRegexTest, LargeFile16G_1M_Buffer)
//{
//  test_file("16g", 1024*1024, 1024LL*1024*1024*16-2);
//}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
