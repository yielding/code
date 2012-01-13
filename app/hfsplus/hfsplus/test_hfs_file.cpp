#include "hfs_file.h"

#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <fstream>

using namespace std;
using namespace boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSFileTest: public testing::Test
{
protected:
  virtual void SetUp()
  {
  }

  virtual void TearDown()
  {
  }
};

TEST_F(HFSFileTest, True)
{
  EXPECT_EQ(1, 1);
}

TEST_F(HFSFileTest, TruncateFile)
{
  char const* fname = "tmp_file";
  ofstream f; f.open(fname, ios_base::binary);
  EXPECT_TRUE(f.is_open());

  for (int i=0; i<100; i++) f.write((char*)&i, 1);

  f.close();

  EXPECT_TRUE(file_size(fname) == 100);

  resize_file(fname, 50);
  EXPECT_TRUE(file_size(fname) == 50);

  remove(fname);
}



////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
