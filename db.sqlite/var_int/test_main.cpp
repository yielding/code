#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iostream>

#include "ByteBuffer.h"

using namespace std;
using namespace utility::hex;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class VITest: public testing::Test
{
protected:
  virtual void SetUp()
  {
  }

  virtual void TearDown()
  {
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
string hex_(uint8_t v)
{
  char buffer[10] = { 0 };
  sprintf(buffer, "%02x", v);

  return string(buffer);
}

void to_hex(uint8_t* buffer, size_t size)
{
  for (auto i=0; i<size; i++) 
    cout << hex_(buffer[i]) << " ";

  cout << endl;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
/*
   TEST_F(VITest, OneByte)
   {
   uint8_t buffer[10] = { 0 };

   auto s0 = sqlite3PutVarint(buffer, 0xff);
   to_hex(buffer, s0);

   auto s1 = sqlite3PutVarint(buffer, 0xffff);
   to_hex(buffer, s1);

   auto s2 = sqlite3PutVarint(buffer, 0xffffff);
   to_hex(buffer, s2);

   auto s3 = sqlite3PutVarint(buffer, 0xffffffff);
   to_hex(buffer, s3);

   auto s4 = sqlite3PutVarint(buffer, 0xffffffffff);
   to_hex(buffer, s4);

   auto s5 = sqlite3PutVarint(buffer, 0xffffffffffff);
   to_hex(buffer, s5);

   auto s6 = sqlite3PutVarint(buffer, 0xffffffffffffff);
   to_hex(buffer, s6);

   auto s7 = sqlite3PutVarint(buffer, 0xffffffffffffffff);
   to_hex(buffer, s7);

   EXPECT_EQ(1, 1);
   }
   */

TEST_F(VITest, set_varint3)
{
  { ByteBuffer b(8);
    b.set_varint(0xffffff);
    b.set_varint(0xffffff);
    b.flip();
    EXPECT_EQ(b.get_varint(), 0xffffff);  
    EXPECT_EQ(b.get_varint(), 0xffffff); }

  try
  {
    ByteBuffer b(7);
    b.set_varint(0xffffff);
    b.set_varint(0xffffff);
    b.flip();
    EXPECT_EQ(b.get_varint(), 0xffffff);
    ASSERT_THROW(b.get_varint(), std::runtime_error);
  }
  catch(std::runtime_error& e)
  {
    // cout << "caught : " << e.what() << endl;
  }
}

TEST_F(VITest, set_varint2)
{
  {
    ByteBuffer b(6);
    b.set_varint(0xfff0);
    b.set_varint(0xffff);
    b.flip();

    EXPECT_EQ(b.get_varint(), 0xfff0);
    EXPECT_EQ(b.get_varint(), 0xffff);
  }

  try
  {
    ByteBuffer b(5);
    b.set_varint(0xfff0);
    b.set_varint(0xffff);
    b.flip();

    EXPECT_EQ(b.get_varint(), 0xfff0);
    ASSERT_THROW(b.get_varint(), std::runtime_error);
  }
  catch(std::runtime_error& e)
  {
    // cout << "caught : " << e.what() << endl;
  }
}

TEST_F(VITest, set_varint1)
{
  ByteBuffer b(0x0f*2);
  for (int i=1; i<=0x0f; i++)
    b.set_varint(0xf0 + i);

  b.flip();

  for (int i=1; i<=0x0f; i++)
    EXPECT_EQ(b.get_varint(), 0xf0 + i);
}

TEST_F(VITest, get_varint9)
{
  uint8_t buffer[] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffffffffffffffff);
}

TEST_F(VITest, get_varint8)
{
  uint8_t buffer[] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffffffffffffff);
}

TEST_F(VITest, get_varint7)
{
  uint8_t buffer[] = { 0xbf, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffffffffffff);
}

TEST_F(VITest, get_varint6)
{
  uint8_t buffer[] = { 0x9f, 0xff, 0xff, 0xff, 0xff, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffffffffff);
}

TEST_F(VITest, get_varint5)
{
  uint8_t buffer[] = { 0x8f, 0xff, 0xff, 0xff, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffffffff);
}

TEST_F(VITest, get_varint4)
{
  uint8_t buffer[] = { 0x87, 0xff, 0xff, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffffff);
}

TEST_F(VITest, get_varint3)
{
  uint8_t buffer[] = { 0x83, 0xff, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xffff);
}

TEST_F(VITest, get_varint2)
{
  uint8_t buffer[] = { 0x81, 0x7f };
  const int sz = sizeof(buffer) / sizeof(buffer[0]);
  ByteBuffer b(buffer, sz);
  EXPECT_EQ(b.get_varint(), 0xff);
}

TEST_F(VITest, TurtleWithMock)
{
  //    int n = 100;
  //    MockTurtle turtle;
  //    EXPECT_CALL(turtle, GetX())
  //        .WillOnce(Return(100))
  //        .WillOnce(Return(200))
  //        .WillOnce(Return(300));

  //    EXPECT_EQ(turtle.GetX(), 100);
  //    EXPECT_EQ(turtle.GetX(), 200);
  //    EXPECT_EQ(turtle.GetX(), 300);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{
  ::testing::InitGoogleMock(&argc, argv);

  return RUN_ALL_TESTS();
}
