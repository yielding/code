#include <iostream>
#include <vector>
#include <numeric>
#include <string>
#include <functional>

using namespace std;

int main(int argc, char *argv[])
{
  vector<int> v{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  auto sum = accumulate(v.begin(), v.end(), 0);

  auto product = accumulate(v.begin(), v.end(), 1, multiplies<int>());

  auto s = accumulate(next(v.begin()), v.end(), 
      to_string(v[0]),
      [](string a, int b) { return a + '-' + to_string(b); }
  );

  cout << "sum: " << sum << '\n'
       << "product: " << product << '\n'
       << "dash-separated string: " << s << '\n';


  return 0;
}
