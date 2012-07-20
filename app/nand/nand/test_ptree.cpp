#include <gtest/gtest.h>
#include <string>
#include <iostream>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

using namespace std;
using namespace boost::property_tree;

class PTreeTest: public testing::Test 
{
public:
    PTreeTest()
    {
      string path = "/Users/yielding/code/app/nand/nand/resource/d0686b9ba2.plist";
      ifstream ifs(path.c_str());
      read_xml(ifs, m_ptree);
    }

protected:
    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }

protected:
    ptree m_ptree;
};

TEST_F(PTreeTest, NodeInTheDeep)
{
  /*
  auto c1 = m_ptree.get_child("plist.dict.dict");
  cout << "sss: " << c1.front().first << endl;
  cout << "sss: " << c1.front().second.data() << endl;
  cout << c1.size();
  */
  cout << "[" ;
  auto bounds = m_ptree.equal_range("plist.dict");
  for (auto it=bounds.first; it!=bounds.second; ++it)
  {
    cout << it->first << endl;
    cout << it->second.data() << endl;
  }

  cout << "]\n" ;
}

TEST_F(PTreeTest, RootNode)
{
    auto v1 = m_ptree.front().first;
    auto v2 = m_ptree.back().first;
    auto v3 = m_ptree.begin()->first;
    EXPECT_EQ(v1, v2);
    EXPECT_EQ(v2, v3);
    EXPECT_EQ(v3, "plist");
}

TEST_F(PTreeTest, NodeWithAttribute)
{
  /*
    <plist version="1.0">
      <key>
      <dict></dict>

      <key>
      <dict></dict>
    </plist>
  */

  auto c1 = m_ptree.get_child("plist");
  EXPECT_EQ(c1.size(), 2);

  // c1.front().first : the key of first child
  EXPECT_EQ(c1.front().first, "<xmlattr>");

  // c1.front().second.front().first : the key of first grand child
  EXPECT_EQ(c1.front().second.front().first, "version");

  // (firt, second.data()) == (key, value) 
  EXPECT_EQ(c1.front().second.front().second.data(), "1.0");

  EXPECT_EQ(c1.back().first, "dict");
}
