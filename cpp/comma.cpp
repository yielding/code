#include <iostream> 
#include <string> 
#include <sstream>

using namespace std;

string insert_separator(string& s, char separator = ',', int width = 3) 
{ 
  string res = s;
  string::iterator i = res.end() - width; 

  for (; i>res.begin(); i -= width) i = res.insert(i, separator); 

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

int main(int argc, char* argv[]) 
{ 
  string s("123412345"); 
  string r = insert_separator(s); 
  std::cout << "r : " << r << std::endl; 
  return 0;
}
