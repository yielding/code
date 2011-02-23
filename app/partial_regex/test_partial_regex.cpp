#include <gtest/gtest.h>

#include <fstream>
#include <boost/format.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/iostreams/stream.hpp>  // io::stream

#include "partial_regex.h"
#include "filebase_source.h"

using namespace std;
namespace io = boost::iostreams;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class PartialRegexTest: public testing::Test
{
protected:
  virtual void SetUp()
  {                      
  }

  virtual void TearDown()
  {
  }

  void test_file(string const& size, int64_t buffer_size, int64_t end_offset);

  void test_buffer(int64_t buffer_size, int64_t end_offset)
  {}

  PartialRegex m_regex;
  // FileBaseSource m_fbsrc;
};

void PartialRegexTest::test_file(string const& size, int64_t buffer_size, int64_t end_offset)
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

/* Memory Stream begin */
TEST_F(PartialRegexTest, Search0)
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

TEST_F(PartialRegexTest, Search1)
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

TEST_F(PartialRegexTest, Search2)
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

TEST_F(PartialRegexTest, SearchPartial1)
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

TEST_F(PartialRegexTest, SearchPartial1_1)
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

TEST_F(PartialRegexTest, SearchPartial2)
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

TEST_F(PartialRegexTest, SearchOr)
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

TEST_F(PartialRegexTest, SearchOr2)
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

TEST_F(PartialRegexTest, SearchGroup)
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

TEST_F(PartialRegexTest, Escape)
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

TEST_F(PartialRegexTest, SearchPartial3)
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

TEST_F(PartialRegexTest, SearchPartial4)
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

TEST_F(PartialRegexTest, BinarySearch)
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
TEST_F(PartialRegexTest, FileBase1)
{
  FileBase* fb = new FileBase;
  FileBaseSource fbs(fb);
  io::stream<FileBaseSource> in(fbs);

  m_regex.buffer_size(1024*16);
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 0);
  ASSERT_EQ(r[0].length, 2);
  ASSERT_EQ(r[1].offset, 1024*1024-2);
  ASSERT_EQ(r[1].length, 2);

  delete fb;
}


///* FILE begin */
//
//TEST_F(PartialRegexTest, LargeFile1M_PerLine)
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
//TEST_F(PartialRegexTest, LargeFile1M_16K_Buffer)
//{
//  test_file("1m", 1024*16, 1024*1024-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile2M_16K_Buffer)
//{
//  test_file("2m", 1024*16, 1024*1024*2-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile3M_16K_Buffer)
//{
//  test_file("3m", 1024*16, 1024*1024*3-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile10M_16K_Buffer)
//{
//  test_file("10m", 1024*16, 1024*1024*10-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile100M_16K_Buffer)
//{
//  test_file("100m", 1024*16, 1024*1024*100-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile200M_4K_Buffer)
//{
//  test_file("200m", 1024*16, 1024*1024*200-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile500M_16K_Buffer)
//{
//  test_file("500m", 1024*16, 1024*1024*500-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile1G_16K_Buffer)
//{
//  test_file("1g", 1024*16, 1024*1024*1024-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile2G_16K_Buffer)
//{
//  test_file("2g", 1024*16, 1024*1024*1024*2-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile1G_32K_Buffer)
//{
//  test_file("1g", 1024*32, 1024*1024*1024-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile1G_64K_Buffer)
//{
//  test_file("1g", 1024*64, 1024*1024*1024-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile512K16G__Buffer)
//{
//  test_file("16g", 1024*512, 1024LL*1024*1024*16-2);
//}
//
//TEST_F(PartialRegexTest, LargeFile16G_1M_Buffer)
//{
//  test_file("16g", 1024*1024, 1024LL*1024*1024*16-2);
//}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
