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

TEST_F(ByteBufferTest, FromHexCode)
{
    uint16_t arr[] = { 
        0x92, 0xa7, 0x42, 0xab, 0x08, 0xc9, 0x69, 0xbf, 
        0x00, 0x6c, 0x94, 0x12, 0xd3, 0xcc, 0x79, 0xa5 };

    auto b = ByteBuffer::from_hexcode("92a742ab08c969bf006c9412d3cc79a5");
    for (int i=0; i<b.size(); i++)
        EXPECT_EQ(int(b[i]), arr[i]);
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
