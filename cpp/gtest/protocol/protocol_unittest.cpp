#include <gtest/gtest.h>
#include "protocol.h"

class ProtocolTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    m_rfid = new rfid_protocol;
    m_rtls = new rtls_protocol;
  }

  virtual void TearDown()
  {
    delete m_rfid;
    delete m_rtls;
  }

protected:
  rfid_protocol* m_rfid;
  rtls_protocol* m_rtls;
};

TEST_F(ProtocolTest, Init)
{
  EXPECT_EQ("rfid_protocol", m_rfid->name());
  EXPECT_EQ("rtls_protocol", m_rtls->name());
}

TEST_F(ProtocolTest, Clone)
{
  protocol *p1 = m_rfid->clone();
  EXPECT_EQ("rfid_protocol", p1->name());

  protocol *pp1 = p1->clone();
  EXPECT_EQ("rfid_protocol", pp1->name());

  protocol *p2 = m_rtls->clone();
  EXPECT_EQ("rtls_protocol", p2->name());

  protocol *pp2 = p2->clone();
  EXPECT_EQ("rtls_protocol", pp2->name());

  delete p1; delete pp1;
  delete p2; delete pp2;
}
