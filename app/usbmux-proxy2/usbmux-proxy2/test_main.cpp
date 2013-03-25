#ifdef __APPLE__
#define GTEST_USE_OWN_TR1_TUPLE 1 //See issue tracker #5
#endif

#include "BPlist.h"
#include "BPlistRepr.h"
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iostream>

using namespace std;
using namespace utility::parser;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class USBMux2Test: public testing::Test
{
protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }
};

TEST_F(USBMux2Test, ListenPacket)
{
  CFDictionary dict;
  dict.add("ClientVersionString", "usbmux2 by yielding");
  dict.add("MessageType", "Listen");
  dict.add("ProgName", "tcprelay");

  PropertyList plist;
  auto s = plist.set(dict).to_xml();
  cout << s << endl;
  cout << "length: " << s.length() << endl;
}

TEST_F(USBMux2Test, XXXX)
{
    EXPECT_EQ(1, 1);
}

TEST_F(USBMux2Test, TurtleWithMock)
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
