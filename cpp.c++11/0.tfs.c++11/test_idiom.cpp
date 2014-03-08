#include "common.h"

#include <boost/lambda/lambda.hpp>

TEST(Idiom, EraseRemove)
{
  string a = "12,345,678";
  auto is_comma = [](char a) { return a == ','; };
  a.erase(remove_if(a.begin(), a.end(), is_comma), a.end());

  string b = "12345678";
  ASSERT_THAT(a.size(), Eq(b.length()));
  ASSERT_THAT(a, Eq(b));
}

TEST(Idiom, EraseRemoveBoost)
{
  // Boost lambda is polymorphic
  using namespace boost::lambda;

  string a = "12,345,678";
  a.erase(remove_if(a.begin(), a.end(), _1 == ','), a.end());

  string b = "12345678";
  ASSERT_THAT(a.size(), Eq(b.length()));
  ASSERT_THAT(a, Eq(b));
}
