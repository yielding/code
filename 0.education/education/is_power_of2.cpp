#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;

bool is_power_of_tow(int no)
{
  return no > 0 && 
         (no & (no - 1)) == 0;
}

int main()
{
  vector<int> powers { 1, 2, 4, 8, 16, 32, 64, 128, 256 };

  for (auto& no: powers)
    cout << boolalpha << is_power_of_tow(no) << endl;

  return 0;
}
