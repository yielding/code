#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <iostream>

using namespace std;

class Turtle
{
public:
  virtual void PenUp() = 0;
  virtual void PenDown() = 0;
  virtual void Forward(int distance) = 0;
  virtual void Turn(int degrees) = 0;
  virtual void GoTo(int x, int y) = 0;
  virtual int  GetX() const = 0;
  virtual int  GetY() const = 0;
};

class MockTurtle: public Turtle
{
public:
  MOCK_METHOD0(PenUp,      void());
  MOCK_METHOD0(PenDown,    void());
  MOCK_METHOD1(Forward,    void(int distance));
  MOCK_METHOD1(Turn,       void(int degrees));
  MOCK_METHOD2(GoTo,       void(int x, int y));
  MOCK_CONST_METHOD0(GetX, int());
  MOCK_CONST_METHOD0(GetY, int());
};


class ConcretePacketStream
{
public:
  void AppendPacket(Packet* new_packet);
  auto GetPacket(size_t packet_no) const -> const Packet*;
  auto NumberofPackets() const -> size_t;
};

template <typename PacketStream> 
class PacketReader
{
public:
  void ReadPackets(PacketStream* stream, size_t packet_no);
};


using ::testing::AtLeast;

TEST(TurtleTest, DrawTurtle)
{
  MockTurtle turtle;

  EXPECT_CALL(turtle, PenDown())
    .Times(AtLeast(1));

  turtle.PenDown();

  EXPECT_TRUE(1);
}

int main(int argc, char* argv[])
{
  ::testing::InitGoogleMock(&argc, argv);
  return RUN_ALL_TESTS();
}
