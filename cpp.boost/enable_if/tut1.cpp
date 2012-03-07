#include <iostream>
#include <string>

#include <boost/utility.hpp>
#include <boost/type_traits/is_arithmetic.hpp>

using namespace std;
using namespace boost;

template <typename T> 
T add(T a, T b, typename enable_if<is_arithmetic<T>>::type* dummpy=0)
{
  return a + b;
}

/*
template <typename T> 
T add(T a, T b)
{
  return a + b;
}
*/

int main(int argc, char const* argv[])
{
  cout << add(1, 2);

//  string a="a";
//  string b="b";
//  cout << add(a, b);
  
  return 0;
}
