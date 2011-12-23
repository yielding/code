#include "hfs_volume.h"

#include <boost/filesystem.hpp>
#include <gtest/gtest.h>

using namespace std;
using namespace utility::hex;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSHeaderTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
    auto b = read(0x400, 0x400); 
    m_header.read_from(b);
  }

public:
  ByteBuffer read(int64_t offset, size_t sz)
  {
    ByteBuffer b(sz);

    ifstream stream;
    stream.open("data/HFSPlus.dmg", ios_base::binary);
    if (!stream.is_open())
      return b;

    stream.seekg(offset, ios_base::beg);
    if (stream.fail())
      throw runtime_error("HFSVolume::read error");

    stream.read((char*)(uint8_t*)b, sz);

    return b;
  }

protected:
  HFSPlusVolumeHeader m_header;

};

TEST_F(HFSHeaderTest, True)
{
  EXPECT_EQ(1, 1);
}

TEST_F(HFSHeaderTest, ReadVolumeHeader)
{
  auto sig = m_header.signature;
  EXPECT_TRUE(sig == 0x482b || sig == 0x4858);
  if (sig == 0x482b)      // HFS+
  {
    EXPECT_EQ(m_header.version, 4);
  }
  else if (sig == 0x4858) // HFSX
  {
    EXPECT_EQ(m_header.version, 5);
  }
  else
  {
    EXPECT_TRUE(0);
  }

  auto lm = m_header.lastMountedVersion;
  EXPECT_TRUE((lm == 0x4846534A) || (lm == 0x6673636b));

  auto sz = m_header.totalBlocks * m_header.blockSize;
  EXPECT_EQ(fs::file_size("data/HFSPlus.dmg"), sz);


}

TEST_F(HFSHeaderTest, ReadVolumeJournal)
{
  auto jb_pos = m_header.journalInfoBlock; // (0x11 * 0x1000).to_s(16) == 0x11000
  auto sz = m_header.blockSize;
  auto offset = jb_pos * sz;
  auto b0 = read(offset, sz);
  JournalInfoBlock jib; jib.read_from(b0);

  EXPECT_EQ(jib.flags, 1);
  auto b1 = read(jib.offset, jib.size);
  journal_header jh; jh.read_from(b1);

  EXPECT_EQ(jib.offset, offset + 0x1000);
  EXPECT_EQ(jh.magic , 0x4a4e4c78);   // xLNJ
  EXPECT_EQ(jh.endian, 0x12345678);   // xV4
  EXPECT_EQ(jh.size, 0x800000);

  auto sector_size = jh.jhdr_size;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
