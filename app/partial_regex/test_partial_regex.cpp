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

TEST_F(PartialRegexTest, Search0)
{
  string text = "123456789012345678bcd23456789012345678901234567890";
          //     |        |        ^|         |         |         |
          //     0        10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  ASSERT_NE(m_regex.search(boost::regex("abc"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.empty(), true);
}

TEST_F(PartialRegexTest, Search1)
{
  string text = "123abc789012345678bcd23456789012345678901234567890";
          //     |  ^     |        ^|         |         |         |
          //     0        10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  ASSERT_EQ(m_regex.search(boost::regex("abc"), in), true);
  match_results const& r = m_regex.result();
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
  ASSERT_EQ(m_regex.search(boost::regex("abc"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.size(), 2);
  ASSERT_EQ(r[0].offset, 3);
  ASSERT_EQ(r[0].length, 3);
  ASSERT_EQ(r[1].offset, 44);
  ASSERT_EQ(r[1].length, 3);
}

TEST_F(PartialRegexTest, SearchPartial1)
{
  string text = "123abc789012345678bcd23456789012345678901234abc890";
          //     |  ^      |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  ASSERT_EQ(m_regex.search(boost::regex("bcd"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 18);
  ASSERT_EQ(r[0].length, 3);
}

TEST_F(PartialRegexTest, BinarySearch)
{
  string text = "12345678901234567890123456789012345678901234567890";
          //     |         |         |         |         |         |
          //     0         10        20        30        40        50
  text[ 8] = 0xFF;
  text[ 9] = 0xFF;
  text[10] = 0xFF;
  text[11] = 0xFD;
  io::filtering_istream in(boost::make_iterator_range(text));
  ASSERT_EQ(m_regex.search(boost::regex("\xFF{3}\xFD"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 8);
  ASSERT_EQ(r[0].length, 4);
}

TEST_F(PartialRegexTest, SearchPartial2)
{
  string text = "123abc789012345678bcd23456789012345678xxxxx4abc890";
          //     |  ^      |       ^ |         |         |         |
          //     0         10        20        30        40        50
  io::filtering_istream in(boost::make_iterator_range(text));
  ASSERT_EQ(m_regex.search(boost::regex("x{5}"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 38);
  ASSERT_EQ(r[0].length, 5);
}

//TEST_F(PartialRegexTest, LargeFile1M)
//{
//  std::fstream in;
//  in.open("test_1m.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024);
//  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
//  match_results const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 1048574); // 
//  ASSERT_EQ(r[0].length, 2);
//}
//
//TEST_F(PartialRegexTest, LargeFile1G_64K_Buffer)
//{
//  std::fstream in;
//  in.open("test_1g.bin", ios_base::binary | ios_base::in);
//  m_regex.buffer_size(1024*64);
//  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
//  match_results const& r = m_regex.result();
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
//  match_results const& r = m_regex.result();
//  ASSERT_EQ(r.size(), 1);
//  ASSERT_EQ(r[0].offset, 1024*1024*1024-2); // 
//  ASSERT_EQ(r[0].length, 2);
//}

// my choice
TEST_F(PartialRegexTest, LargeFile1G_16K_Buffer)
{
  std::fstream in;
  in.open("test_1g.bin", ios_base::binary | ios_base::in);
  m_regex.buffer_size(1024*16);
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 1024*1024*1024-2); // 
  ASSERT_EQ(r[0].length, 2);
}

TEST_F(PartialRegexTest, LargeFile16G_16K_Buffer)
{
  std::fstream in;
  in.open("test_16g.bin", ios_base::binary | ios_base::in);
  m_regex.buffer_size(1024*16);
  ASSERT_EQ(m_regex.search(boost::regex("\xFF\xFF"), in), true);
  match_results const& r = m_regex.result();
  ASSERT_EQ(r.size(), 1);
  ASSERT_EQ(r[0].offset, 1024*1024*1024*16-2); // 
  ASSERT_EQ(r[0].length, 2);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
