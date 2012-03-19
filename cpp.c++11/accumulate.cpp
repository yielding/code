#include <algorithm>
#include <vector>
#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  vector<double> v = { 1.0, 2.0, 3.0, 4.0, 5.0 };

  auto x = [=](double so_far, double x) { 
    return so_far + x * x; 
  };

  cout << accumulate(v.begin(), v.end(), 0.0, x) << endl;
  cout << inner_product(v.begin(), v.end(), v.begin(), 0) << endl;

  return 0;
}