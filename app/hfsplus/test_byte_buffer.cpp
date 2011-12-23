#include "ByteBuffer.h"

#include <gtest/gtest.h>
#include <iostream>

using namespace std;
using namespace utility::hex;

class ByteBufferTest: public testing::Test
{
protected:
    virtual void SetUp()
    {
        m_buffer = new ByteBuffer;
    }

    virtual void TearDown()
    {
        delete m_buffer;
    }

    ByteBuffer* m_buffer;
};

TEST_F(ByteBufferTest, Size)
{
    EXPECT_EQ(m_buffer->size(), 0);
    EXPECT_EQ(m_buffer->capacity(), 0);
}

TEST_F(ByteBufferTest, SizeCapicity)
{
    EXPECT_EQ(m_buffer->size(), 0);

    uint32_t data[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    int size = sizeof data / sizeof data[0];

    for (int i=0; i<size; ++i)
        m_buffer->set_uint4_net(data[i]);

    EXPECT_EQ(m_buffer->size(), 40);

    EXPECT_TRUE(m_buffer->capacity() >= 40);
}

TEST_F(ByteBufferTest, Append)
{
    uint8_t data[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    ByteBuffer b0(data, 10);
    ByteBuffer b1(data, 10);

    b0.append(b1);
    EXPECT_EQ(b0.size(), 20);
    b0.append(data, 10);
    EXPECT_EQ(b0.size(), 30);
}

TEST_F(ByteBufferTest, NetSet2)
{
    uint16_t data[] = { 1, 2, 3, 4, 5 }; 

    int size = sizeof data / sizeof data[0];

    for (int i=0; i<size; ++i)
    {
        m_buffer->set_uint2_net(data[i]);
    }

    for (int i=0; m_buffer->has_remaining(); i++)
    {
        uint16_t value = m_buffer->get_int2_net();
        EXPECT_EQ(value, data[i]);
        EXPECT_EQ(m_buffer->remaining(), (size - i - 1) * 2);
    }
}

TEST_F(ByteBufferTest, NetSet4)
{
    uint32_t data[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    int size = sizeof data / sizeof data[0];

    for (int i=0; i<size; ++i)
        m_buffer->set_uint4_net(data[i]);

    m_buffer->flip();
    for (int i=0; m_buffer->has_remaining(); i++)
    {
        uint32_t value = m_buffer->get_uint4_net();
        EXPECT_EQ(value, data[i]);
        EXPECT_EQ(m_buffer->remaining(), (size - i - 1) * 4);
    }
}

TEST_F(ByteBufferTest, SetStringWithEmptyNull)
{
    char const* msg = NULL;
    char const* msg1 = "";
    m_buffer->set_string(msg);
    m_buffer->set_string(msg);
    m_buffer->flip();

    EXPECT_EQ(strcmp(m_buffer->c_str(), msg1), 0);
    EXPECT_EQ(strcmp(m_buffer->c_str(), msg1), 0);
}

TEST_F(ByteBufferTest, SetStringWithEmptyString)
{
    char const* msg = "";

    m_buffer->set_string(msg);
    m_buffer->set_string(msg);
    m_buffer->flip();

    EXPECT_EQ(strcmp(m_buffer->c_str(), msg), 0);
    EXPECT_EQ(strcmp(m_buffer->c_str(), msg), 0);
}

TEST_F(ByteBufferTest, SetString)
{
    char const* msg = "abcdefghijklmnopqrstuvwxyz";

    m_buffer->set_string(msg);
    m_buffer->set_string(msg);
    m_buffer->flip();

    EXPECT_EQ(strcmp(m_buffer->c_str(), msg), 0);
    EXPECT_EQ(strcmp(m_buffer->c_str(), msg), 0);
}

TEST_F(ByteBufferTest, GetString1)
{
    char const* msg = "abcdefghijklmnopqrstuvwxyz";
    m_buffer->set_string(msg);
    m_buffer->flip();
    EXPECT_EQ(m_buffer->get_string(0), string(""));
    m_buffer->flip();
    EXPECT_EQ(m_buffer->get_string(3), string("abc"));
    m_buffer->flip();
    EXPECT_EQ(m_buffer->get_string(), string(msg));
}

TEST_F(ByteBufferTest, GetString2)
{
    uint8_t src[] = {'a', 'b', 'c', 0, 'd', 0, 'e', 'f', 'g' };
    size_t size = sizeof src / sizeof src[0];
    m_buffer->set_binary(src, size);
    m_buffer->flip();

    EXPECT_EQ(m_buffer->get_string(), string("abc"));
    EXPECT_EQ(m_buffer->get_string(), string("d"));
    EXPECT_EQ(m_buffer->get_string(), string("efg"));
}

TEST_F(ByteBufferTest, SetBinary)
{
    uint8_t arr[] = { 1, 2, 3, 4, 5 };
    int size = sizeof arr / sizeof arr[0];
    ByteBuffer buffer;
    buffer.set_binary(arr, 5);

    buffer.flip();
    uint8_t* res = buffer.get_binary(size);
    for (int i=0; i<size; i++)
        EXPECT_EQ(*(res + i), arr[i]);
}

TEST_F(ByteBufferTest, Peek1At)
{
    m_buffer->set_uint1(1);
    m_buffer->flip();
    uint8_t v = m_buffer->peek1_at(0);
    EXPECT_EQ(v, 1);
}

TEST_F(ByteBufferTest, Exception)
{
    bool have_exception = false;

    try
    {
        m_buffer->get_uint1();
    }
    catch (runtime_error& e)
    {
        have_exception = true;
        ASSERT_STREQ(e.what(),  "get_uint1(): index out of range");
    }
    catch (...)
    {
        have_exception = true;
    }

    EXPECT_TRUE(have_exception);
}
