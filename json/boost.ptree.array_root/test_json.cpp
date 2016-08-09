#include <gtest/gtest.h>

#include <boost/property_tree/json_parser.hpp>
#include <string>
#include <iostream>
#include <sstream>
#include <map>

using namespace std;
      namespace pt = boost::property_tree;

class JsonReadTest: public testing::Test 
{
public:
  JsonReadTest()
  {}

protected:
  virtual void SetUp() 
  {
    istringstream iss(R"(
    [
      {
        "app_id": 54,
        "ver_id": 2514,
        "app_name": "KakaoTalk",
        "db_path": "4141AF8D-619D-4096-BDAE-987B723B9345"
      }
    ]
    )");

    pt::json_parser::read_json(iss, tree);
  }

  virtual void TearDown() {
  }

  pt::ptree tree;
};

TEST_F(JsonReadTest, AlwaysTrue)
{
  int count = 0;

  map<string, string> result;
  
  for (auto& element: tree)
  {
    count += 1;
    for (auto& property: element.second)
      result[property.first] = property.second.get_value<string>();
  }

  EXPECT_EQ(count, 1);
  EXPECT_EQ(result["app_id"], "54");
  EXPECT_EQ(result["ver_id"], "2514");
  EXPECT_EQ(result["app_name"], "KakaoTalk");
  EXPECT_EQ(result["db_path"], "4141AF8D-619D-4096-BDAE-987B723B9345");
}
