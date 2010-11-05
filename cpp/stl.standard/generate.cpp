#include <algorithm>
#include <vector>	

#include <iostream>

//
// Value generator simply doubles the current value and returns it.
//
template <class T>
class generate_val
{
private:
  T val_;
public:
  generate_val(const T& val) : val_(val) {}
  T& operator()() { val_ += val_; return val_; }
};

int main ()
{
  using namespace std;

  int d1[4] = {1,2,3,4};
  generate_val<int> gen(1);
  //
  // Set up two vectors.
  //
  vector<int> v1(d1+0, d1+4), v2(d1+0, d1+4);
  //
  // Set up one empty vector.
  //
  vector<int> v3;
  //
  // Generate values for all of v1.
  //
  generate(v1.begin(), v1.end(), gen);
  //
  // Generate values for first 3 of v2.
  //
  generate_n(v2.begin(), 3, gen);
  //
  // Use insert iterator to generate 5 values for v3.
  //
  generate_n(back_inserter(v3), 5, gen);
  //
  // Copy all three to cout.
  //
  ostream_iterator<int> out(cout," ");
  copy(v1.begin(), v1.end(), out);
  cout << endl;
  copy(v2.begin(), v2.end(), out);
  cout << endl;
  copy(v3.begin(), v3.end(), out);
  cout << endl;
  //
  // Generate 3 values for cout.
  //
  generate_n(ostream_iterator<int>(cout," "), 3, gen);
  cout << endl;

  return 0;
}
