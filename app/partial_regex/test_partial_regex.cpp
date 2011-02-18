#include <gtest/gtest.h>

#include <fstream>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/range/iterator_range.hpp>

#include "partial_regex.h"

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
    m_text = "123abc789012345678bcd23456789012345678901234abc890";
    //       |   ^     |        *|         |         |    ^    |
  }

  virtual void TearDown()
  {
  }

  string m_text;
  PartialRegex m_regex;
};
//
//TEST_F(PartialRegexTest, Search0)
//{
//  string text = "123456789012345678bcd23456789012345678901234567890";
//          //     |        |        ^|         |         |         |
//          //     0        10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_NE(m_regex.search(boost::regex("abc"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.empty(), true);
//}
//
//TEST_F(PartialRegexTest, Search1)
//{
//  string text = "123abc789012345678bcd23456789012345678901234567890";
//          //     |  ^     |        ^|         |         |         |
//          //     0        10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("abc"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 3);
//  ASSERT_EQ(r[0].length, 3);
//}
//
//TEST_F(PartialRegexTest, Search2)
//{
//  string text = "123abc789012345678bcd23456789012345678901234abc890";
//          //     |  ^      |       ^ |         |         |         |
//          //     0         10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("abc"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 3);
//  ASSERT_EQ(r[0].length, 3);
//  ASSERT_EQ(r[1].offset, 44);
//  ASSERT_EQ(r[1].length, 3);
//}
//
//TEST_F(PartialRegexTest, SearchPartial1)
//{
//  string text = "123456789bc2345678bcd23456789012345678901234abc890";
//          //     |         |       ^ |         |         |         |
//          //     0         10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 18);
//  ASSERT_EQ(r[0].length, 3);
//}
//
//TEST_F(PartialRegexTest, SearchPartial1_1)
//{
//  string text = "123456789bc23bcd78bcd23456789012345678901234abc890";
//  //     |         |       ^ |         |         |         |
//  //     0         10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 13);
//  ASSERT_EQ(r[0].length, 3);
//  ASSERT_EQ(r[1].offset, 18);
//  ASSERT_EQ(r[1].length, 3);
//}
//
//TEST_F(PartialRegexTest, SearchPartial2)
//{
//  string text = "123456789012345678bcd23456789012345678901234abc890";
//          //     |         |       ^ |         |         |         |
//          //     0         10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 18);
//  ASSERT_EQ(r[0].length, 3);
//}
//
//TEST_F(PartialRegexTest, SearchPartial3)
//{
//  string text = "123abc789012345678bcd23456789012345678xxxxx4abc890";
//          //     |  ^      |       ^ |         |         |         |
//          //     0         10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("x{5}"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 38);
//  ASSERT_EQ(r[0].length, 5);
//}
//
//TEST_F(PartialRegexTest, SearchPartial4)
//{
//  string text = "xx3456789x123456789x123456789x123456789x12345678xx";
//          //     |  ^      |       ^ |         |         |         |
//          //     0         10        20        30        40        50
//  io::filtering_istream in(boost::make_iterator_range(text));
//  m_regex.buffer_size(10);
//  ASSERT_EQ(m_regex.search(boost::regex("xx"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(text.length(), 50);
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0);
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, 48);
//  ASSERT_EQ(r[1].length, 2);
//}
//
//TEST_F(PartialRegexTest, BinarySearch)
//{
//  string text = "12345678901234567890123456789012345678901234567890";
//          //     |         |         |         |         |         |
//          //     0         10        20        30        40        50
//  text[18] = 0xff;
//  text[19] = 0xff;
//  text[20] = 0xff;
//  text[21] = 0xfd;
//  io::filtering_istream in(boost::make_iterator_range(text));
//  for (int size=16; size<=4096; size+=16)
//  {
//    m_regex.buffer_size(size);
//    ASSERT_EQ(m_regex.search(boost::regex("\xff{3}\xfd"), in), true);
//    matches const& r = m_regex.result();
//    ASSERT_EQ(r.size(), 1);
//    ASSERT_EQ(r[0].offset, 18);
//    ASSERT_EQ(r[0].length, 4);
//
//    ASSERT_EQ(m_regex.search(boost::regex("\xff\xff\xff\xfd"), in), true);
//    matches const& s = m_regex.result();
//    ASSERT_EQ(s.size(), 1);
//    ASSERT_EQ(s[0].offset, 18);
//    ASSERT_EQ(s[0].length, 4);
//  }
//}
//
// FILE begin
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
//TEST_F(PartialRegexTest, LargeFile1M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_1m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 0xffffe;
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}
//
//TEST_F(PartialRegexTest, LargeFile2M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_2m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 1024*1024*2 -2 ;
//
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}
//
//TEST_F(PartialRegexTest, LargeFile3M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_3m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 1024*1024*3 -2 ;
//
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}
//
//TEST_F(PartialRegexTest, LargeFile10M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_10m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 1024*1024*10 -2 ;
//
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}
//
//TEST_F(PartialRegexTest, LargeFile100M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_100m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 1024*1024*100 - 2 ;
//
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}
//
//TEST_F(PartialRegexTest, LargeFile200M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_200m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 1024*1024*200 - 2 ;
//
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}
//
//TEST_F(PartialRegexTest, LargeFile500M_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_500m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& r = m_regex.result();
//  int64_t offset_ = 1024*1024*500 - 2 ;
//
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}

TEST_F(PartialRegexTest, LargeFile1G_4K_Buffer)
{
  std::fstream in;
  in.open("test_1g.bin", ios_base::binary | ios_base::in);
  m_regex.buffer_size(1024*16);
  int64_t offset_ = 1024*1024*1024LL-2;

  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 0); // 
  ASSERT_EQ(r[0].length, 2);
  ASSERT_EQ(r[1].offset, offset_);
  ASSERT_EQ(r[1].length, 2);

  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
  matches const& s = m_regex.result();
  ASSERT_EQ(s.size(), 2);
  ASSERT_EQ(s[0].offset, 0); // 
  ASSERT_EQ(s[0].length, 2);
  ASSERT_EQ(s[1].offset, offset_);
  ASSERT_EQ(s[1].length, 2);

  in.close();
}

//TEST_F(PartialRegexTest, LargeFile2G_4K_Buffer)
//{
//  std::fstream in;
//  in.open("test_2g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  int64_t offset_ = 1024LL * 1024 * 1024 * 2 - 2;
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff\xff"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 2);
//  ASSERT_EQ(r[0].offset, 0); // 
//  ASSERT_EQ(r[0].length, 2);
//  ASSERT_EQ(r[1].offset, offset_);
//  ASSERT_EQ(r[1].length, 2);
//
//  ASSERT_EQ(m_regex.search(boost::regex("\xff{2}"), in), true);
//  matches const& s = m_regex.result();
//  ASSERT_EQ(s.size(), 2);
//  ASSERT_EQ(s[0].offset, 0); // 
//  ASSERT_EQ(s[0].length, 2);
//  ASSERT_EQ(s[1].offset, offset_);
//  ASSERT_EQ(s[1].length, 2);
//
//  in.close();
//}

//TEST_F(PartialRegexTest, LargeFile1G_64K_Buffer)
//{
//  std::fstream in;
//  in.open("test_1g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*64);
//  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 1024*1024*1024-2); // 
//  ASSERT_EQ(r[0].length, 2);
//}
//
//TEST_F(PartialRegexTest, LargeFile1G_32K_Buffer)
//{
//  std::fstream in;
//  in.open("test_1g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*32);
//  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 1024*1024*1024-2); // 
//  ASSERT_EQ(r[0].length, 2);
//}

// my choice
//TEST_F(PartialRegexTest, LargeFile1G_16K_Buffer)
//{
//  std::fstream in;
//  in.open("test_1g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*16);
//  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
//  matches const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 1024*1024*1024-2); // 
//  ASSERT_EQ(r[0].length, 2);
//}

TEST_F(PartialRegexTest, LargeFile16G_16K_Buffer)
{
  std::fstream in;
  in.open("test_16g.bin", ios_base::binary | ios_base::in);
  m_regex.buffer_size(1024*16);
  int16_t offset_ = 1024LL*1024*1024*16-2;
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  matches const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 0);
  ASSERT_EQ(r[0].length, 2);
  ASSERT_EQ(r[1].offset, offset_);
  ASSERT_EQ(r[1].length, 2);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
