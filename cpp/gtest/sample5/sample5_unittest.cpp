#include <limits.h>
#include <time.h>
#include "sample3-inl.h"
#include <gtest/gtest.h>
#include "sample1.h"
#include <unistd.h>

class QuickTest : public testing::Test 
{
protected:
  virtual void SetUp() {
    start_time_ = time(NULL);
  }

  virtual void TearDown()
  {
    const time_t end_time = time(NULL);

    EXPECT_TRUE(end_time - start_time_ <= 5) << "The test took too long.";
  }

  time_t start_time_;
};

class IntegerFunctionTest: public QuickTest
{
};


TEST_F(IntegerFunctionTest, Factorial)
{
  EXPECT_EQ(1, Factorial(-5));
  EXPECT_EQ(1, Factorial(-1));
  EXPECT_TRUE(Factorial(-10) > 0);

  EXPECT_EQ(1, Factorial(0));

  EXPECT_EQ(1, Factorial(1));
  EXPECT_EQ(2, Factorial(2));
  EXPECT_EQ(6, Factorial(3));
  EXPECT_EQ(40320, Factorial(8));
}

TEST_F(IntegerFunctionTest, IsPrime)
{
  // Tests negative input.
  EXPECT_TRUE(!IsPrime(-1));
  EXPECT_TRUE(!IsPrime(-2));
  EXPECT_TRUE(!IsPrime(INT_MIN));

  // Tests some trivial cases.
  EXPECT_TRUE(!IsPrime(0));
  EXPECT_TRUE(!IsPrime(1));
  EXPECT_TRUE(IsPrime(2));
  EXPECT_TRUE(IsPrime(3));

  // Tests positive input.
  EXPECT_TRUE(!IsPrime(4));
  EXPECT_TRUE(IsPrime(5));
  EXPECT_TRUE(!IsPrime(6));
  EXPECT_TRUE(IsPrime(23));
}


// The next test case (named "QueueTest") also needs to be quick, so
// we derive another fixture from QuickTest.
//
// The QueueTest test fixture has some logic and shared objects in
// addition to what's in QuickTest already.  We define the additional
// stuff inside the body of the test fixture, as usual.
class QueueTest: public QuickTest
{
protected:
  virtual void SetUp()
  {
    // First, we need to set up the super fixture (QuickTest).
    QuickTest::SetUp();

    // Second, some additional setup for this fixture.
    q1_.Enqueue(1);
    q2_.Enqueue(2);
    q2_.Enqueue(3);
  }

  // By default, TearDown() inherits the behavior of
  // QuickTest::TearDown().  As we have no additional cleaning work
  // for QueueTest, we omit it here.
  //
  // virtual void TearDown() {
  //   QuickTest::TearDown();
  // }

  Queue<int> q0_;
  Queue<int> q1_;
  Queue<int> q2_;
};


TEST_F(QueueTest, DefaultConstructor)
{
  EXPECT_EQ(0, q0_.Size());
}

TEST_F(QueueTest, Dequeue)
{
  int * n = q0_.Dequeue();
  EXPECT_TRUE(n == NULL);

  n = q1_.Dequeue();
  EXPECT_TRUE(n != NULL);
  EXPECT_EQ(1, *n);
  EXPECT_EQ(0, q1_.Size());
  delete n;

  n = q2_.Dequeue();
  EXPECT_TRUE(n != NULL);
  EXPECT_EQ(2, *n);
  EXPECT_EQ(1, q2_.Size());
  delete n;
}
