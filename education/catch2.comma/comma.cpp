#include <iostream> 
#include <string> 
#include <sstream>

#include "catch2/catch.hpp"

using namespace std;

string insert_separator(string s, char separator = ',', int width=3) 
{ 
  auto res(s);
  auto i = res.end() - width; 

  for (; i>res.begin(); i -= width) 
      i = res.insert(i, separator); 

  return res;
} 

template <typename T> 
string to_s(const T& t)
{
  ostringstream s;
  s << t;

  return s.str();
}

template <typename T> 
string toString(const T& t)
{
  ostringstream s;
  s << t;

  return s.str();
}

TEST_CASE("Insert comma") {
  auto s = "123412345"s;
  auto r = insert_separator(s); 

  REQUIRE (r == "123,412,345");
}

/*
int main(int argc, char* argv[]) 
{ 
  string s("123412345"); 
  string r = insert_separator(s); 
  std::cout << "r : " << r << std::endl; 
  return 0;
}
*/
