#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SCPTest: public testing::Test
{
protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }
};

TEST_F(SCPTest, XXXXYYY)
{
    EXPECT_EQ(1, 2);
}

TEST_F(SCPTest, XXXX)
{
    EXPECT_EQ(1, 1);
}

TEST_F(SCPTest, TurtleWithMock)
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
