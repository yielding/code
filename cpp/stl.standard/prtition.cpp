#include<functional>
#include<deque>
#include<algorithm>

#include <iostream>

using namespace std;

//
// Create a new predicate from unary_function.
//
template<class Arg>
class is_even : public unary_function<Arg, bool>
{
public:
  bool operator()(const Arg& arg1) { return (arg1 % 2) == 0; } 
};

int main ()
{
  //
  // Initialize a deque with an array of integers.
  //
  int init[10] = { 1,2,3,4,5,6,7,8,9,10 };
  deque<int,allocator<int> > d1(init+0, init+10);
  deque<int,allocator<int> > d2(init+0, init+10);
  //
  // Print out the original values.
  //
  cout << "Unpartitioned values: " << "\t\t";
  copy(d1.begin(), d1.end(), ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << endl;
  //
  // A partition of the deque according to even/oddness.
  //
  partition(d2.begin(), d2.end(), is_even<int>());
  //
  // Output result of partition.
  //
  cout << "Partitioned values: " << "\t\t";
  copy(d2.begin(), d2.end(), ostream_iterator<int>(cout," "));
  cout << endl;
  //
  // A stable partition of the deque according to even/oddness.
  //
  stable_partition(d1.begin(), d1.end(), is_even<int>());
  //
  // Output result of partition.
  //
  cout << "Stable partitioned values: " << "\t";
  copy(d1.begin(), d1.end(), ostream_iterator<int>(cout," "));
  cout << endl;

  return 0;
}
