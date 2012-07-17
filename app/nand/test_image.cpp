#include <gtest/gtest.h>

class NANDImageFlatTest: public testing::Test 
{
protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }
};


TEST_F(NANDImageFlatTest, True)
{
    EXPECT_EQ(1, 1);
}
