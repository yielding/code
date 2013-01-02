#include <gtest/gtest.h>
#include <string>
#include <iostream>

#include "ByteBuffer.h"

using namespace std;
using namespace utility::hex;

class ByteBufferTest: public testing::Test 
{
public:
    ByteBufferTest()
    {}

protected:
    virtual void SetUp()
    {}

    virtual void TearDown()
    {}
};

TEST_F(ByteBufferTest, ConstructBeginEnd)
{
    uint8_t arr[] = { 'l', 'e', 'e', 'c', 'h' };
    ByteBuffer b(arr, arr + 5);
    EXPECT_EQ(b.to_s(), "leech");
}

TEST_F(ByteBufferTest, FromHexCode)
{
    uint8_t arr[] = { 
        0x92, 0xa7, 0x42, 0xab, 0x08, 0xc9, 0x69, 0xbf, 
        0x00, 0x6c, 0x94, 0x12, 0xd3, 0xcc, 0x79, 0xa5 };

    auto b = ByteBuffer::from_hexcode("92a742ab08c969bf006c9412d3cc79a5");
    for (int i=0; i<b.size(); i++)
        EXPECT_EQ(uint8_t(b[i]), arr[i]);
}

TEST_F(ByteBufferTest, StartsWith)
{
    ByteBuffer b("leech");
    EXPECT_EQ(b.starts_with("lee"), true);
    EXPECT_EQ(b.starts_with("kee"), false);
}

TEST_F(ByteBufferTest, ToS)
{
    ByteBuffer b("leech");

    EXPECT_EQ(b.to_s(), "leech");
}

TEST_F(ByteBufferTest, SetUint2LE)
{
    ByteBuffer b;
    uint32_t value = 0x0102;
    b.set_uint2_le(value);

    EXPECT_EQ(int(b[0]), 2);
    EXPECT_EQ(int(b[1]), 1);
}

TEST_F(ByteBufferTest, SetUint4LE)
{
    ByteBuffer b;
    uint32_t value = 0x01020304;
    b.set_uint4_le(value);

    for (int i=0; i<4; i++)
        EXPECT_EQ(int(b[i]), 4 - i);
}

TEST_F(ByteBufferTest, LastN)
{
    ByteBuffer b("leech");
    auto res0 = b.last();
    auto res1 = b.last(3);
    auto res2 = b.last(5);

    EXPECT_EQ(res0, uint8_t('h'));
    EXPECT_EQ(res1.to_s(), string("ech"));
    EXPECT_EQ(res2.to_s(), string("leech"));
}

TEST_F(ByteBufferTest, FirstN)
{
    ByteBuffer b("leech");

    auto res1 = b.first(3);
    auto res2 = b.first(5);
    EXPECT_EQ(res1.to_s(), string("lee"));
    EXPECT_EQ(res2.to_s(), string("leech"));
}

TEST_F(ByteBufferTest, Reverse)
{
    ByteBuffer b("leech");
    ByteBuffer c("leech ");

    b.reverse();
    auto res = b.to_s();
    EXPECT_EQ(b.to_s(), res);
    EXPECT_FALSE(c.to_s() == res);
}

TEST_F(ByteBufferTest, Slice)
{
    ByteBuffer a("0123456789");

    auto s = a.slice(4, 8);
    EXPECT_EQ(s.to_s(), string("4567"));
    EXPECT_EQ(a.offset(), 0);
}

TEST_F(ByteBufferTest, Take)
{
    ByteBuffer a("0123456789");
    auto t1 = a.take(2); EXPECT_EQ(t1.to_s(), string("01"));
    auto t2 = a.take(2); EXPECT_EQ(t2.to_s(), string("23"));
    auto t3 = a.take(2); EXPECT_EQ(t3.to_s(), string("45"));
    EXPECT_EQ(a.offset(), 6);
}
