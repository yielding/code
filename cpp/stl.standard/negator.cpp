#include<functional>
#include<algorithm>

#include <iostream>

using namespace std;

using std::unary_function;
//
// Create a new predicate from unary_function.
//
template<class Arg>
class is_odd : public unary_function<Arg, bool>
{
public:
  bool operator() (const Arg& arg1) const
  {
    return (arg1 % 2 ? true : false);
  }
};

int main ()
{
  using namespace std;

  less<int> less_func;
  //
  // Use not2 on less.
  //
  cout << (less_func(1,4) ? "TRUE" : "FALSE") << endl;
  cout << (less_func(4,1) ? "TRUE" : "FALSE") << endl;
  cout << (not2(less<int>())(1,4) ? "TRUE" : "FALSE") << endl;
  cout << (not2(less<int>())(4,1) ? "TRUE" : "FALSE") << endl;   
  //
  // Create an instance of our predicate.
  //
  is_odd<int> odd;
  //
  // Use not1 on our user defined predicate.
  //
  cout << (odd(1) ? "TRUE" : "FALSE") << endl;
  cout << (odd(4) ? "TRUE" : "FALSE") << endl;
  cout << (not1(odd)(1) ? "TRUE" : "FALSE") << endl;
  cout << (not1(odd)(4) ? "TRUE" : "FALSE") << endl;

  return 0;
}
