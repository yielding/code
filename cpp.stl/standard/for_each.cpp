#include <vector>
#include <algorithm>
#include <functional>

#include <iostream>

using namespace std;

template <class Arg>
class out_times_x :  private unary_function<Arg,void>
{
private:
  Arg multiplier;
public:
  out_times_x(const Arg& x) : multiplier(x) { }
  void operator()(const Arg& x) { cout << x * multiplier << " " << endl; }
};

int main ()
{
  int sequence[5] = {1,2,3,4,5};  
  //
  // Set up a vector.
  //
  vector<int> v(sequence+0, sequence+5);
  // 
  // Setup a function object.
  //
  out_times_x<int> f2(2);

  for_each(v.begin(), v.end(), f2);   // Apply function

  return 0;
}
