#include <gtest/gtest.h>
#include <string>
#include <iostream>

#include "byte_buffer2.h"

using namespace std;

class ByteBuffer2Test: public testing::Test 
{
public:
  ByteBuffer2Test()
  {}

protected:
  virtual void SetUp() {
    arr = new uint8_t[10] {
      0x01, 0x02, 0x03, 0x04, 0x05, 
      0x06, 0x07, 0x08, 0x09, 0x0a
    };

    minus = new uint8_t[10] { 
      0xff, 0xff, 0xff, 0xff, 0xff, 
      0xff, 0xff, 0xff, 0xff, 0 
    };

    src = new uint8_t[5] { 'l', 'e', 'e', 'c', 'h' };
  }

  virtual void TearDown() {
    delete [] arr;
    delete [] src;
  }

  uint8_t* arr;
  uint8_t* minus;
  uint8_t* src;
};

TEST_F(ByteBuffer2Test, CtorWithInit)
{
  ByteBuffer2 b = { 1, 2, 3 };

  EXPECT_EQ(b.offset(), 0);
  EXPECT_EQ(b.size(), 3);
  EXPECT_EQ(b[0], 1);
}

TEST_F(ByteBuffer2Test, ConstructBeginEnd)
{
  ByteBuffer2 b(src, 5);

  EXPECT_EQ(b.to_s(), "leech");
}

TEST_F(ByteBuffer2Test, GetInt8)
{
  ByteBuffer2 b = { 1, 2, 3 };

  EXPECT_EQ(b.get_uint8(2), 3);
  EXPECT_EQ(b.get_int8(2), 3);
  EXPECT_EQ(b.size(), 3);
  EXPECT_EQ(b[0], 1);
}

TEST_F(ByteBuffer2Test, ToS)
{
  ByteBuffer2 b("leech");

  EXPECT_EQ(b.to_s(), "leech");
  EXPECT_EQ(b.offset(), 0);
}

TEST_F(ByteBuffer2Test, HasRemaining)
{
  ByteBuffer2 b(arr, 0, 10, false);

  for (int i=0; i<10; i++)
  {
    EXPECT_TRUE(b.has_remaining());
    b.get_int8();
  }
}

TEST_F(ByteBuffer2Test, GetInt16BE)
{
  ByteBuffer2 b(arr, 0, 5);

  EXPECT_EQ(0x0102, b.get_int16_be());
  EXPECT_EQ(0x0304, b.get_int16_be());
  ASSERT_THROW(b.get_int16_be(), out_of_range);
  b.reset();
  EXPECT_EQ(0x0102, b.get_uint16_be());
  EXPECT_EQ(0x0304, b.get_uint16_be());
  ASSERT_THROW(b.get_int16_be(), out_of_range);

  b.reset();
  ByteBuffer2 m(minus, 0, 5);
  EXPECT_EQ(-1, m.get_int16_be());
  EXPECT_EQ(2,  m.offset());
}

TEST_F(ByteBuffer2Test, GetInt24BE)
{
  ByteBuffer2 b(arr, 0, 6);

  EXPECT_EQ(0x010203, b.get_int24_be());
  EXPECT_EQ(0x040506, b.get_int24_be());
  b.reset();
  EXPECT_EQ(0x010203, b.get_uint24_be());
  EXPECT_EQ(0x040506, b.get_uint24_be());

  b.reset();
  ByteBuffer2 m(minus, 0, 6);
  EXPECT_EQ(-1, m.get_int24_be());
  EXPECT_EQ(-1, m.get_int24_be());
  ASSERT_THROW(m.get_int24_be(), out_of_range);
}

TEST_F(ByteBuffer2Test, GetInt32BE)
{
  ByteBuffer2 b(arr, 0, 8);

  EXPECT_EQ(0x01020304, b.get_int32_be());
  EXPECT_EQ(0x05060708, b.get_int32_be());
  ASSERT_THROW(b.get_int16_be(), out_of_range);

  b.reset();
  EXPECT_EQ(0x01020304, b.get_uint32_be());
  EXPECT_EQ(0x05060708, b.get_uint32_be());
  ASSERT_THROW(b.get_int16_be(), out_of_range);

  b.reset();
  ByteBuffer2 m(minus, 0, 5);
  EXPECT_EQ(-1, m.get_int32_be());
  EXPECT_EQ(4,  m.offset());
}

TEST_F(ByteBuffer2Test, GetInt40BE)
{
  ByteBuffer2 b(arr, 0, 8);

  EXPECT_EQ(0x0102030405, b.get_int40_be());

  b.reset();
  EXPECT_EQ(0x0102030405, b.get_uint40_be());

  b.reset();
  EXPECT_EQ(0x0504030201, b.get_int40_le());

  b.reset();
  EXPECT_EQ(0x0504030201, b.get_uint40_le());

  b.reset();
  ByteBuffer2 m(minus, 0, 8);
  EXPECT_EQ(-1, m.get_int40_be());
  ASSERT_THROW(m.get_int40_be(), out_of_range);
}

TEST_F(ByteBuffer2Test, GetInt48BE)
{
  ByteBuffer2 b(arr, 0, 8);

  EXPECT_EQ(0x010203040506, b.get_int48_be());

  b.reset();
  EXPECT_EQ(0x010203040506, b.get_uint48_be());

  b.reset();
  EXPECT_EQ(0x060504030201, b.get_int48_le());

  b.reset();
  EXPECT_EQ(0x060504030201, b.get_uint48_le());

  b.reset();
  ByteBuffer2 m(minus, 0, 8);
  EXPECT_EQ(-1, m.get_int48_be());
  ASSERT_THROW(m.get_int48_be(), out_of_range);
}

TEST_F(ByteBuffer2Test, GetInt56BE)
{
  ByteBuffer2 b(arr, 0, 8);

  EXPECT_EQ(0x01020304050607, b.get_int56_be());

  b.reset();
  EXPECT_EQ(0x01020304050607, b.get_uint56_be());

  b.reset();
  EXPECT_EQ(0x07060504030201, b.get_int56_le());

  b.reset();
  EXPECT_EQ(0x07060504030201, b.get_uint56_le());

  b.reset();
  ByteBuffer2 m(minus, 0, 8);
  EXPECT_EQ(-1, m.get_int56_be());
  ASSERT_THROW(m.get_int56_be(), out_of_range);
}

TEST_F(ByteBuffer2Test, GetInt64BE)
{
  ByteBuffer2 b(arr, 0, 8);

  EXPECT_EQ(0x0102030405060708, b.get_int64_be());
  ASSERT_THROW(b.get_int16_be(), out_of_range);

  b.reset();
  EXPECT_EQ(0x0102030405060708, b.get_uint64_be());
  ASSERT_THROW(b.get_int16_be(), out_of_range);

  b.reset();
  ByteBuffer2 m(minus, 0, 8);
  EXPECT_EQ(-1, m.get_int64_be());
  EXPECT_EQ(8,  m.offset());
}

TEST_F(ByteBuffer2Test, VarInt)
{
  int size = 0;
  int64_t r0 = 0;

  ByteBuffer2 b0 = { 0x81, 0x7f };

  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(2,  size);
  EXPECT_EQ(0xff, r0);

  b0.reset({ 0x83, 0xff, 0x7f });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(3,  size);
  EXPECT_EQ(0xffff, r0);

  b0.reset({ 0x87, 0xff, 0xff, 0x7f });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(4,  size);
  EXPECT_EQ(0xffffff, r0);

  b0.reset({ 0x8f, 0xff, 0xff, 0xff, 0x7f });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(5,  size);
  EXPECT_EQ(0xffffffff, r0);

  b0.reset({ 0x9f, 0xff, 0xff, 0xff, 0xff, 0x7f });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(6,  size);
  EXPECT_EQ(0xffffffffff, r0);

  b0.reset({ 0xbf, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(7,  size);
  EXPECT_EQ(0xffffffffffff, r0);

  b0.reset({ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(8,  size);
  EXPECT_EQ(0xffffffffffffff, r0);

  b0.reset({ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff });
  r0 = b0.get_varint_with_size(&size);
  EXPECT_EQ(9,  size);
  EXPECT_EQ(0xffffffffffffffff, r0);

  b0.reset();
  r0 = b0.get_varint();
  EXPECT_EQ(0xffffffffffffffff, r0);
}

TEST_F(ByteBuffer2Test, Skip)
{
  ByteBuffer2 b(arr, 0, 10);
  EXPECT_EQ(2, b.skip(2).offset());
}

TEST_F(ByteBuffer2Test, Take)
{
  ByteBuffer2 a("0123456789");
  EXPECT_EQ(a.take(2).to_s(), string("01"));
  EXPECT_EQ(a.take(2).to_s(), string("23"));
  EXPECT_EQ(a.take(2).to_s(), string("45"));
  EXPECT_EQ(a.offset(), 6);
}

TEST_F(ByteBuffer2Test, FirstN)
{
  ByteBuffer2 b("leech");

  auto res1 = b.first(3);
  auto res2 = b.first(5);
  EXPECT_EQ(res1.to_s(), string("lee"));
  EXPECT_EQ(res2.to_s(), string("leech"));
}

TEST_F(ByteBuffer2Test, LastN)
{
  ByteBuffer2 b("leech");

  EXPECT_EQ(b.last(), uint8_t('h'));
  EXPECT_EQ(b.last(3).to_s(), string("ech"));
  EXPECT_EQ(b.last(5).to_s(), string("leech"));
}

TEST_F(ByteBuffer2Test, FromHexCode)
{
  uint8_t arr[] = { 
    0x92, 0xa7, 0x42, 0xab, 0x08, 0xc9, 0x69, 0xbf, 
    0x00, 0x6c, 0x94, 0x12, 0xd3, 0xcc, 0x79, 0xa5 };

  auto b = ByteBuffer2::from_hexcode("92a742ab08c969bf006c9412d3cc79a5");
  for (int i=0; i<b.size(); i++) 
    EXPECT_EQ( arr[i] , uint8_t(b[i]));
}

TEST_F(ByteBuffer2Test, Slice)
{
  ByteBuffer2 a("0123456789");

  auto s0 = a.slice(4, 4);
  EXPECT_EQ(s0.to_s(), string("4567"));
  EXPECT_EQ(a.offset(), 0);

  auto s1 = a.slice(4, 0);
  EXPECT_EQ(s1.to_s(), string(""));
  EXPECT_EQ(a.offset(), 0);
}

TEST_F(ByteBuffer2Test, StartsWith)
{
  ByteBuffer2 b("leech");
  EXPECT_TRUE(b.starts_with("lee"));
  EXPECT_FALSE(b.starts_with("kee"));
  EXPECT_THROW(b.starts_with("leeche"), out_of_range);
}

TEST_F(ByteBuffer2Test, GetDouble)
{
  ByteBuffer2 bb = { 0x3f, 0xf3, 0xae, 0x14, 0x7a, 0xe1, 0x47, 0xae };
  EXPECT_EQ(bb.get_double(), 1.23);
}

TEST_F(ByteBuffer2Test, GetHexString)
{
  ByteBuffer2 bb = { 0x00, 0x01, 0x02, 0x03};
  EXPECT_EQ(bb.get_hex_string(3), "000102"s);
  EXPECT_EQ(bb.offset(), 3);
}
