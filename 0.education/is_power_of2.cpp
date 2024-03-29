#include <vector>
#include <iostream>
#include <iomanip>
#include <cassert>

using namespace std;

bool is_power_of_two(int no)
{
  return no > 0 && 
         (no & (no - 1)) == 0;
}

int main()
{
  vector<int> powers { 1, 2, 4, 8, 16, 32, 64, 128, 256 };

  for (auto& no: powers)
  {
    assert(is_power_of_two(no));
    // cout << boolalpha << is_power_of_two(no) << endl;
  }

  return 0;
}