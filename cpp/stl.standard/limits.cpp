#include <limits>

#include <iostream>

int main() 
{
  using namespace std;

  numeric_limits<float> float_info;
  if (float_info.is_specialized && float_info.has_infinity)
  {
    //
    // Get value of infinity.
    //
    cout << float_info.infinity() << endl; 
  }
  return 0;
}
